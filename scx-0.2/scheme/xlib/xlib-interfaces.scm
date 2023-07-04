(define :enumeration :syntax)
(define :display :value)
(define :x-error :value)

(define-interface xlib-internal-interface
  (export
   display:after-function set-display:after-function!

   (queued-mode :enumeration)
   events-queued events-pending
   wait-event
   event-ready?
   next-event peek-event
   ))

(define-interface xlib-interface
  (export
   ;; display.scm ****************************************************
   display? display:connection-number display:protocol-version
   display:protocol-revision display:server-vendor display:image-byte-order
   display:bitmap-unit display:bitmap-pad display:bitmap-bit-order
   display:vendor-release display:queue-length display:name
   display:default-screen display:screens display-message-inport
   display:error-queue

   (byte-order :syntax) byte-order?
   (bit-order :syntax) bit-order?

   screen-format? screen-format:depth screen-format:bits-per-pixel
   screen-format:scanline-pad

   screen?
   screen:display screen:root-window screen:width screen:height
   screen:width-mm screen:height-mm screen:number screen:root-depth
   screen:default-visual screen:default-gc screen:default-colormap
   screen:white-pixel screen:black-pixel screen:max-maps
   screen:min-maps screen:does-backing-store screen:does-save-unders?
   screen:event-mask

   screen-number-of-screen

   open-display
   close-display

   none parent-relative copy-from-parent pointer-window input-focus
   pointer-root any-property-type any-key all-temporary
   current-time no-symbol all-planes

   display:last-request-read
   default-root-window
   white-pixel black-pixel
   next-request
   synchronize
   set-after-function!
   display-flush
   display-sync
   display-no-op
   display-select-input

   ;; window.scm *****************************************************
   create-window
   create-simple-window

   change-window-attributes
   (bit-gravity :syntax) bit-gravity?
   (win-gravity :syntax) win-gravity?
   (backing-store :syntax) backing-store?
   (set-window-attribute :syntax) set-window-attribute?
   (make-set-window-attribute-alist :syntax)
   set-window-background-pixmap! set-window-background-pixel!
   set-window-border-pixmap! set-window-border-pixel!
   set-window-bit-gravity! set-window-gravity!
   set-window-backing-store! set-window-backing-planes!
   set-window-backing-pixel! set-window-save-under!
   set-window-event-mask! set-window-do-not-propagate-mask!
   set-window-override-redirect! set-window-colormap!
   set-window-cursor!

   configure-window
   (stack-mode :syntax) stack-mode?
   (window-change :syntax) window-change?
   (make-window-change-alist :syntax)
   set-window-x! set-window-y! set-window-width! set-window-height!
   set-window-border-width! set-window-sibling! set-window-stack-mode!
   move-window resize-window move-resize-window

   get-window-attributes
   (map-state :syntax) map-state?
   (window-class :syntax) window-class?
   window-attributes? window-attribute:x
   window-attribute:y window-attribute:width
   window-attribute:height window-attribute:border-width
   window-attribute:depth window-attribute:visual
   window-attribute:root window-attribute:class
   window-attribute:bit-gravity window-attribute:gravity
   window-attribute:backing-store window-attribute:backing-planes
   window-attribute:backing-pixel window-attribute:save-under
   window-attribute:colormap window-attribute:map-installed
   window-attribute:map-state window-attribute:all-event-masks
   window-attribute:your-event-mask window-attribute:do-not-propagate-mask
   window-attribute:override-redirect window-attribute:screen

   get-geometry
   window-x window-y
   window-width window-height
   window-border-width window-depth

   map-window map-raised map-subwindows
   unmap-window unmap-subwindows

   destroy-window destroy-subwindows

   raise-window lower-window
   (circulate-direction :syntax)
   circulate-direction?
   circulate-subwindows
   circulate-subwindows-up
   circulate-subwindows-down

   restack-windows
   clear-area
   clear-window
   query-tree
   window-root
   window-parent
   window-children
   translate-coordinates
   query-pointer-root
   query-pointer-state
   query-pointer

   window-exists?

   ;; colormap.scm ***************************************************
   make-color color? color:pixel set-color:pixel!
   color:red set-color:red! color:green set-color:green!
   color:blue set-color:blue!
   
   (colormap-state :syntax) colormap-state?
   (colormap-alloc :syntax) colormap-alloc?
   create-colormap
   copy-colormap-and-free
   free-colormap
   
   alloc-color! alloc-color alloc-named-color/exact
   alloc-named-color alloc-color-cells/planes alloc-color-cells
   alloc-color-planes
   
   free-colors query-colors! query-colors
   query-color! query-color lookup-color
   parse-color

   store-colors store-color store-named-color

   ;; cursor.scm *****************************************************
   create-pixmap-cursor create-glyph-cursor create-font-cursor
   define-cursor undefine-cursor free-cursor
   recolor-cursor
   ((xc-X-cursor xc-arrow xc-based-arrow-down xc-based-arrow-up
     xc-boat xc-bogosity xc-bottom-left-corner xc-bottom-right-corner
     xc-bottom-side xc-bottom-tee xc-box-spiral xc-center-ptr
     xc-circle xc-clock xc-coffee-mug xc-cross xc-cross-reverse
     xc-crosshair xc-diamond-cross xc-dot xc-dotbox xc-double-arrow
     xc-draft-large xc-draft-small xc-draped-box xc-exchange
     xc-fleur xc-gobbler xc-gumby xc-hand1 xc-hand2 xc-heart
     xc-icon xc-iron-cross xc-left-ptr xc-left-side xc-left-tee
     xc-leftbutton xc-ll-angle xc-lr-angle xc-man xc-middlebutton
     xc-mouse xc-pencil xc-pirate xc-plus xc-question-arrow
     xc-right-ptr xc-right-side xc-right-tee xc-rightbutton
     xc-rtl-logo xc-sailboat xc-sb-down-arrow xc-sb-h-double-arrow
     xc-sb-left-arrow xc-sb-right-arrow xc-sb-up-arrow
     xc-sb-v-double-arrow xc-shuttle xc-sizing xc-spider xc-spraycan
     xc-star xc-target xc-tcross xc-top-left-arrow xc-top-left-corner
     xc-top-right-corner xc-top-side xc-top-tee xc-trek xc-ul-angle
     xc-umbrella xc-ur-angle xc-watch xc-xterm) :number)
   
   ;; error.scm ******************************************************
   x-error? x-error:display x-error:serial x-error:code
   x-error:major-opcode x-error:minor-opcode x-error:resource-id
   x-error:text

   (error-code :syntax) error-code?
   use-x-error-warnings!

   x-error-queue? x-error-queue:this
   empty-x-error-queue?
   next-x-error-queue

   get-error-text
   get-error-database-text

   ((set-fatal-error-handler!) (proc ((proc (:display) :value))
				     (proc (:display) :value)))

   ;; sync-event.scm *************************************************
   init-sync-x-events
   sync-x-event? sync-x-event-event
   next-sync-x-event
   most-recent-sync-x-event

   call-with-event-channel

   ;; event-types.scm ************************************************
   (event-type :enumeration)
   (event-mask :syntax)

   (notify-mode :enumeration)
   (notify-detail :enumeration)
   (visibility-state :enumeration)
   (place :enumeration)
   (property-state :enumeration)
   (property-format :enumeration)
   (mapping-request :enumeration)

   any-event-type
   any-event-serial
   any-event-send-event?
   any-event-display
   any-event-window

   create-key-event
   key-event?
   key-event-type
   key-event-serial
   key-event-send-event?
   key-event-display
   key-event-window
   key-event-root
   key-event-subwindow
   key-event-time
   key-event-x
   key-event-y
   key-event-x-root
   key-event-y-root
   key-event-state
   key-event-keycode
   key-event-same-screen?
   
   create-button-event
   button-event?
   button-event-type
   button-event-serial
   button-event-send-event?
   button-event-display
   button-event-window
   button-event-root
   button-event-subwindow
   button-event-time
   button-event-x
   button-event-y
   button-event-x-root
   button-event-y-root
   button-event-state
   button-event-button
   button-event-same-screen?

   create-motion-event
   motion-event?
   motion-event-type
   motion-event-serial
   motion-event-send-event?
   motion-event-display
   motion-event-window
   motion-event-root
   motion-event-subwindow
   motion-event-time
   motion-event-x
   motion-event-y
   motion-event-x-root
   motion-event-y-root
   motion-event-state
   motion-event-is-hint?
   motion-event-same-screen?
   
   create-crossing-event
   crossing-event?
   crossing-event-type
   crossing-event-serial
   crossing-event-send-event?
   crossing-event-display
   crossing-event-window
   crossing-event-root
   crossing-event-subwindow
   crossing-event-time
   crossing-event-x
   crossing-event-y
   crossing-event-x-root
   crossing-event-y-root
   crossing-event-mode
   crossing-event-detail
   crossing-event-same-screen?
   crossing-event-focus?
   crossing-event-state
   
   create-focus-change-event
   focus-change-event?
   focus-change-event-type
   focus-change-event-serial
   focus-change-event-send-event?
   focus-change-event-display
   focus-change-event-window
   focus-change-event-mode
   focus-change-event-detail
   
   create-expose-event
   expose-event?
   expose-event-type
   expose-event-serial
   expose-event-send-event?
   expose-event-display
   expose-event-window
   expose-event-x
   expose-event-y
   expose-event-width
   expose-event-height
   expose-event-count

   create-graphics-expose-event
   graphics-expose-event?
   graphics-expose-event-type
   graphics-expose-event-serial
   graphics-expose-event-send-event?
   graphics-expose-event-display
   graphics-expose-event-drawable
   graphics-expose-event-x
   graphics-expose-event-y
   graphics-expose-event-width
   graphics-expose-event-height
   graphics-expose-event-major-code
   graphics-expose-event-minor-code

   create-no-expose-event
   no-expose-event?
   no-expose-event-type
   no-expose-event-serial
   no-expose-event-send-event?
   no-expose-event-display
   no-expose-event-drawable
   no-expose-event-major-code
   no-expose-event-minor-code

   create-visibility-event
   visibility-event?
   visibility-event-type
   visibility-event-serial
   visibility-event-send-event?
   visibility-event-display
   visibility-event-window
   visibility-event-state

   create-create-window-event
   create-window-event?
   create-window-event-type
   create-window-event-serial
   create-window-event-send-event?
   create-window-event-display
   create-window-event-parent
   create-window-event-window
   create-window-event-x
   create-window-event-y
   create-window-event-width
   create-window-event-height
   create-window-event-border-width
   create-window-event-override-redirect?

   create-destroy-window-event
   destroy-window-event?
   destroy-window-event-type
   destroy-window-event-serial
   destroy-window-event-send-event?
   destroy-window-event-display
   destroy-window-event-event
   destroy-window-event-window

   create-unmap-event
   unmap-event?
   unmap-event-type
   unmap-event-serial
   unmap-event-send-event?
   unmap-event-display
   unmap-event-event
   unmap-event-window
   unmap-event-from-configure?
   
   create-map-event
   map-event?
   map-event-type
   map-event-serial
   map-event-send-event?
   map-event-display
   map-event-event
   map-event-window
   map-event-override-redirect?
   
   create-map-request-event
   map-request-event?
   map-request-event-type
   map-request-event-serial
   map-request-event-send-event?
   map-request-event-display
   map-request-event-parent
   map-request-event-window

   create-reparent-event
   reparent-event?
   reparent-event-type
   reparent-event-serial
   reparent-event-send-event?
   reparent-event-display
   reparent-event-event
   reparent-event-window
   reparent-event-parent
   reparent-event-x
   reparent-event-y
   reparent-event-override-redirect?
   
   create-configure-event
   configure-event?
   configure-event-type
   configure-event-serial
   configure-event-send-event?
   configure-event-display
   configure-event-event
   configure-event-window
   configure-event-x
   configure-event-y
   configure-event-width
   configure-event-height
   configure-event-border-width
   configure-event-above
   configure-event-override-redirect?

   create-gravity-event
   gravity-event?
   gravity-event-type
   gravity-event-serial
   gravity-event-send-event?
   gravity-event-display
   gravity-event-event
   gravity-event-window
   gravity-event-x
   gravity-event-y
   
   create-resize-request-event
   resize-request-event?
   resize-request-event-type
   resize-request-event-serial
   resize-request-event-send-event?
   resize-request-event-display
   resize-request-event-window
   resize-request-event-width
   resize-request-event-height
   
   create-configure-request-event
   configure-request-event?
   configure-request-event-type
   configure-request-event-serial
   configure-request-event-send-event?
   configure-request-event-display
   configure-request-event-parent
   configure-request-event-window
   configure-request-event-window-change-alist

   create-circulate-event
   circulate-event?
   circulate-event-type
   circulate-event-serial
   circulate-event-send-event?
   circulate-event-display
   circulate-event-event
   circulate-event-window
   circulate-event-place

   create-circulate-request-event
   circulate-request-event?
   circulate-request-event-type
   circulate-request-event-serial
   circulate-request-event-send-event?
   circulate-request-event-display
   circulate-request-event-parent
   circulate-request-event-window
   circulate-request-event-place
   
   create-property-event
   property-event?
   property-event-type
   property-event-serial
   property-event-send-event?
   property-event-display
   property-event-window
   property-event-atom
   property-event-time
   property-event-state

   create-selection-clear-event
   selection-clear-event?
   selection-clear-event-type
   selection-clear-event-serial
   selection-clear-event-send-event?
   selection-clear-event-display
   selection-clear-event-window
   selection-clear-event-selection
   selection-clear-event-time

   create-selection-request-event
   selection-request-event?
   selection-request-event-type
   selection-request-event-serial
   selection-request-event-send-event?
   selection-request-event-display
   selection-request-event-owner
   selection-request-event-requestor
   selection-request-event-selection
   selection-request-event-target
   selection-request-event-property
   selection-request-event-time

   create-selection-event
   selection-event?
   selection-event-type
   selection-event-serial
   selection-event-send-event?
   selection-event-display
   selection-event-requestor
   selection-event-selection
   selection-event-target
   selection-event-property
   selection-event-time

   create-colormap-event
   colormap-event?
   colormap-event-type
   colormap-event-serial
   colormap-event-send-event?
   colormap-event-display
   colormap-event-window
   colormap-event-colormap
   colormap-event-new?
   colormap-event-state

   create-client-message-event
   client-message-event?
   client-message-event-type
   client-message-event-serial
   client-message-event-send-event?
   client-message-event-display
   client-message-event-window
   client-message-event-property
   
   create-mapping-event
   mapping-event?
   mapping-event-type
   mapping-event-serial
   mapping-event-send-event?
   mapping-event-display
   mapping-event-window
   mapping-event-request
   mapping-event-first-keycode
   mapping-event-count

   create-keymap-event
   keymap-event?
   keymap-event-type
   keymap-event-serial
   keymap-event-send-event?
   keymap-event-display
   keymap-event-bit-vector

   ;; event.scm ******************************************************
   get-motion-events
   send-event

   ;; font.scm *******************************************************
   (font-direction :syntax) font-direction?

   char-struct? char-struct:lbearing char-struct:rbearing
   char-struct:width char-struct:ascent char-struct:descent
   char-struct:attributes

   font-struct? font-struct:cpointer font-struct:fid font-struct:direction
   font-struct:min-char-or-byte2 font-struct:max-char-or-byte2
   font-struct:min-byte1 font-struct:max-byte1 font-struct:all-char-exist?
   font-struct:default-char font-struct:properties font-struct:min-bounds
   font-struct:max-bounds font-struct:per-char font-struct:ascent
   font-struct:descent

   load-font unload-font
   query-font load-query-font
   free-font
   get-font-property
   list-fonts list-fonts-with-info
   set-font-path get-font-path

   ;; gcontext.scm ***************************************************
   gc?
   (gc-function :syntax) gc-function?
   (line-style :syntax) line-style?
   (cap-style :syntax) cap-style?
   (join-style :syntax) join-style?
   (fill-style :syntax) fill-style?
   (fill-rule :syntax) fill-rule?
   (subwindow-mode :syntax) subwindow-mode?
   (arc-mode :syntax) arc-mode?
   (gc-value :syntax) gc-value?
   (make-gc-value-alist :syntax)
   all-gc-values 
   (gc-value-set :syntax) gc-value-set? make-gc-value-set

   create-gc copy-gc! copy-gc

   change-gc
   set-gc-function! set-gc-plane-mask! set-gc-foreground!
   set-gc-background! set-gc-line-width! set-gc-line-style!
   set-gc-cap-style! set-gc-join-style! set-gc-fill-style!
   set-gc-fill-rule! set-gc-arc-mode! set-gc-tile! set-gc-stipple!
   set-gc-ts-x-origin! set-gc-ts-y-origin! set-gc-font!
   set-gc-subwindow-mode! set-gc-graphics-exposures!
   set-gc-clip-x-origin! set-gc-clip-y-origin! set-gc-clip-mask!
   set-gc-dash-offset! set-gc-dashes!

   get-gc-values
   gc-gc-function gc-plane-mask gc-foreground gc-background
   gc-line-width gc-line-style gc-cap-style gc-join-style
   gc-fill-style gc-fill-rule gc-arc-mode gc-tile gc-stipple
   gc-ts-x-origin gc-ts-y-origin gc-font gc-subwindow-mode
   gc-graphics-exposures gc-clip-x-origin gc-clip-y-origin
   gc-clip-mask gc-dash-offset gc-dashes

   free-gc
   gcontext-from-gc

   set-line-attributes!
   set-dashes!
   set-clip-origin
   (rectangle-ordering :syntax) rectangle-ordering?
   set-clip-rectangles!

   query-best-cursor
   query-best-tile
   query-best-stipple

   ;; grab.scm *******************************************************
   (grab-mode :syntax) grab-mode?
   (grab-status :syntax) grab-status?
   grab-pointer ungrab-pointer
   change-active-pointer-grab
   (state :syntax) state?
   (state-set :syntax) state-set? make-state-set
   (button :syntax) button?
   grab-button ungrab-button
   grab-keyboard ungrab-keyboard
   grab-key ungrab-key
   (event-mode :syntax) event-mode?
   allow-events
   grab-server ungrab-server

   ;; graphics.scm ***************************************************
   copy-area copy-plane

   draw-point draw-points
   draw-line draw-lines
   (coord-mode :syntax) coord-mode?

   draw-segments make-segment segment?
   segment:x1 set-segment:x1! segment:y1 set-segment:y1!
   segment:x2 set-segment:x2! segment:y2 set-segment:y2!

   draw-rectangle
   draw-rectangles make-rectangle rectangle?
   rectangle:x set-rectangle:x! rectangle:y set-rectangle:y!
   rectangle:width set-rectangle:width! rectangle:height
   set-rectangle:height!

   draw-arc
   draw-arcs make-arc arc? arc:x set-arc:x! arc:y set-arc:y!
   arc:width set-arc:width! arc:height set-arc:height! arc:angle1
   set-arc:angle1! arc:angle2 set-arc:angle2!

   fill-rectangle fill-rectangles
   (polygon-shape :syntax) polygon-shape?
   fill-polygon
   fill-arc fill-arcs

   bounds
   grow-rectangle
   move/resize-rectangle

   ;; key.scm ********************************************************
   change-keyboard-mapping get-keyboard-mapping
   display-keycodes
   set-modifier-mapping get-modifier-mapping
   string->keysym keysym->string
   keycode->keysym keysym->keycode
   convert-case convert-to-lowercase convert-to-uppercase
   lookup-keysym
   refresh-keyboard-mapping
   lookup-string/keysym lookup-string rebind-keysym

   ;; pixmap.scm *****************************************************
   create-pixmap free-pixmap
   read-bitmap-file write-bitmap-file
   create-bitmap-from-data create-pixmap-from-bitmap-data

   ;; property.scm ***************************************************
   make-property property? property:type set-property:type!
   property:format set-property:format!
   property:data set-property:data!
   intern-atom intern-atoms
   get-atom-name get-atom-names
   list-properties
   rotate-window-properties
   delete-property
   get-window-property
   (change-property-mode :syntax) change-property-mode?
   change-property
   get-full-window-property
   string->string-list
   set-selection-owner get-selection-owner
   convert-selection

   ;; text.scm *******************************************************
   draw-image-string draw-image-string-16
   make-text-item text-item? text-item:string text-item:delta text-item:font
   (make-text-items :syntax)
   draw-text draw-text-16
   text-extents text-extents-16

   ;; visual.scm *****************************************************
   visual?
   (visual-class :syntax) visual-class?
   visual-info? visual-info:visual
   visual-info:visualid set-visual-info:visualid!
   visual-info:screen-number set-visual-info:screen-number!
   visual-info:depth set-visual-info:depth!
   visual-info:class set-visual-info:class!
   visual-info:red-mask set-visual-info:red-mask!
   visual-info:green-mask set-visual-info:green-mask!
   visual-info:blue-mask set-visual-info:blue-mask!
   visual-info:bits-per-rgb set-visual-info:bits-per-rgb!
   visual-info:colormap-size set-visual-info:colormap-size!
   empty-visual-info
   get-visual-infos
   match-visual-info
   visualid-from-visual

   ;; wm.scm *********************************************************
   reparent-window
   install-colormap uninstall-colormap
   list-installed-colormaps
   set-input-focus
   (revert-to :syntax) revert-to?
   get-input-focus get-input-focus-window
   general-warp-pointer warp-pointer warp-pointer-relative
   bell
   set-access-control
   change-save-set (save-set :syntax) save-set?
   (close-down-mode :syntax) close-down-mode?
   set-close-down-mode
   kill-client
   get-pointer-mapping
   set-pointer-mapping

   ;; client.scm *****************************************************
   iconify-window
   withdraw-window
   reconfigure-wm-window
   get-wm-command set-wm-command!
   get-wm-protocols set-wm-protocols!
   get-wm-class set-wm-class!
   (initial-state :syntax) initial-state?
   (wm-hint :syntax) wm-hint?
   (make-wm-hint-alist :syntax)
   get-wm-hints set-wm-hints!
   get-transient-for set-transient-for!
   get-text-property set-text-property!
   property->string-list string-list->property
   get-wm-name set-wm-name!
   get-wm-icon-name set-wm-icon-name!
   get-wm-client-machine set-wm-client-machine!
   (size-hint :syntax) size-hint?
   (make-size-hint-alist :syntax)
   get-wm-normal-hints set-wm-normal-hints!
   make-icon-size icon-size?
   icon-size:min-width set-icon-size:min-width!
   icon-size:min-height set-icon-size:min-height!
   icon-size:max-width set-icon-size:max-width!
   icon-size:max-height set-icon-size:max-height!
   icon-size:width-inc set-icon-size:width-inc!
   icon-size:height-inc set-icon-size:height-inc!
   get-icon-sizes set-icon-sizes!

   ;; utility.scm ****************************************************
   xlib-release-4-or-later?
   xlib-release-5-or-later?
   xlib-release-6-or-later?
   get-default
   resource-manager-string
   parse-geometry
   store-buffer store-bytes fetch-buffer fetch-bytes rotate-buffers

   ;; region.scm *****************************************************
   create-region set-region destroy-region
   empty-region? equal-region? point-in-region?
   (rect-in-region-type :enumeration)
   rect-in-region? intersect-region! intersect-region
   union-region! union-region union-rect-with-region! union-rect-with-region
   subtract-region! subtract-region xor-region! xor-region
   offset-region! shrink-region!
   polygon-region clip-box

   ;; atom.scm *******************************************************
   ((XA_PRIMARY XA_SECONDARY XA_ARC XA_ATOM XA_BITMAP XA_CARDINAL
     XA_COLORMAP XA_CURSOR XA_CUT_BUFFER0 XA_CUT_BUFFER1 XA_CUT_BUFFER2
     XA_CUT_BUFFER3 XA_CUT_BUFFER4 XA_CUT_BUFFER5 XA_CUT_BUFFER6
     XA_CUT_BUFFER7 XA_DRAWABLE XA_FONT XA_INTEGER XA_PIXMAP XA_POINT
     XA_RECTANGLE XA_RESOURCE_MANAGER XA_RGB_COLOR_MAP XA_RGB_BEST_MAP
     XA_RGB_BLUE_MAP XA_RGB_DEFAULT_MAP XA_RGB_GRAY_MAP XA_RGB_GREEN_MAP
     XA_RGB_RED_MAP XA_STRING XA_VISUALID XA_WINDOW XA_WM_COMMAND
     XA_WM_HINTS XA_WM_CLIENT_MACHINE XA_WM_ICON_NAME XA_WM_ICON_SIZE
     XA_WM_NAME XA_WM_NORMAL_HINTS XA_WM_SIZE_HINTS XA_WM_ZOOM_HINTS
     XA_MIN_SPACE XA_NORM_SPACE XA_MAX_SPACE XA_END_SPACE XA_SUPERSCRIPT_X
     XA_SUPERSCRIPT_Y XA_SUBSCRIPT_X XA_SUBSCRIPT_Y XA_UNDERLINE_POSITION
     XA_UNDERLINE_THICKNESS XA_STRIKEOUT_ASCENT XA_STRIKEOUT_DESCENT
     XA_ITALIC_ANGLE XA_X_HEIGHT XA_QUAD_WIDTH XA_WEIGHT XA_POINT_SIZE
     XA_RESOLUTION XA_COPYRIGHT XA_NOTICE XA_FONT_NAME XA_FAMILY_NAME
     XA_FULL_NAME XA_CAP_HEIGHT XA_WM_CLASS XA_WM_TRANSIENT_FOR)
    :number)
   ))
