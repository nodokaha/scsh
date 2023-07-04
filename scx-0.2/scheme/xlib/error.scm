;; Copyright (c) 2001-2003 by Norbert Freudemann, David Frese

;; *** x errors ******************************************************

(define-record-type x-error :x-error
  (make-x-error display serial code major-opcode minor-opcode resource-id text)
  x-error?
  (display x-error:display)
  (serial x-error:serial)
  (code x-error:code)
  (major-opcode x-error:major-opcode)
  (minor-opcode x-error:minor-opcode)
  (resource-id x-error:resource-id)
  (text x-error:text))

(define-exported-binding "scx-x-error" :x-error)

(define-enumerated-type error-code :error-code
  error-code? error-codes error-code-name error-code-index
  (success bad-request bad-value bad-window bad-pixmap bad-atom
   bad-cursor bad-font bad-match bad-drawable bad-access bad-alloc
   bad-color bad-gc bad-id-choice bad-name bad-length bad-implementation))

(define-exported-binding "scx-error-code" :error-code)
(define-exported-binding "scx-error-codes" error-codes)

;; *** error exceptions **********************************************

(define (opcode->string oc)
  (case oc
    ((1) "X_CreateWindow")              
    ((2) "X_ChangeWindowAttributes")        
    ((3) "X_GetWindowAttributes")     
    ((4) "X_DestroyWindow")
    ((5) "X_DestroySubwindows")
    ((6) "X_ChangeSaveSet")
    ((7) "X_ReparentWindow")
    ((8) "X_MapWindow")
    ((9) "X_MapSubwindows")
    ((10) "X_UnmapWindow")
    ((11) "X_UnmapSubwindows")
    ((12) "X_ConfigureWindow")  
    ((13) "X_CirculateWindow")  
    ((14) "X_GetGeometry")
    ((15) "X_QueryTree")
    ((16) "X_InternAtom")
    ((17) "X_GetAtomName")
    ((18) "X_ChangeProperty") 
    ((19) "X_DeleteProperty") 
    ((20) "X_GetProperty")
    ((21) "X_ListProperties")
    ((22) "X_SetSelectionOwner")    
    ((23) "X_GetSelectionOwner")    
    ((24) "X_ConvertSelection")   
    ((25) "X_SendEvent")
    ((26) "X_GrabPointer")
    ((27) "X_UngrabPointer")
    ((28) "X_GrabButton")
    ((29) "X_UngrabButton")
    ((30) "X_ChangeActivePointerGrab")
    ((31) "X_GrabKeyboard")
    ((32) "X_UngrabKeyboard")
    ((33) "X_GrabKey")
    ((34) "X_UngrabKey")
    ((35) "X_AllowEvents")       
    ((36) "X_GrabServer")      
    ((37) "X_UngrabServer")        
    ((38) "X_QueryPointer")        
    ((39) "X_GetMotionEvents")           
    ((40) "X_TranslateCoords")                
    ((41) "X_WarpPointer")       
    ((42) "X_SetInputFocus")         
    ((43) "X_GetInputFocus")         
    ((44) "X_QueryKeymap")       
    ((45) "X_OpenFont")    
    ((46) "X_CloseFont")     
    ((47) "X_QueryFont")
    ((48) "X_QueryTextExtents")
    ((49) "X_ListFonts")  
    ((50) "X_ListFontsWithInfo") 
    ((51) "X_SetFontPath") 
    ((52) "X_GetFontPath") 
    ((53) "X_CreatePixmap")        
    ((54) "X_FreePixmap")      
    ((55) "X_CreateGC")    
    ((56) "X_ChangeGC")    
    ((57) "X_CopyGC")  
    ((58) "X_SetDashes")     
    ((59) "X_SetClipRectangles")             
    ((60) "X_FreeGC")  
    ((61) "X_ClearArea")             
    ((62) "X_CopyArea")    
    ((63) "X_CopyPlane")     
    ((64) "X_PolyPoint")     
    ((65) "X_PolyLine")    
    ((66) "X_PolySegment")       
    ((67) "X_PolyRectangle")         
    ((68) "X_PolyArc")   
    ((69) "X_FillPoly")    
    ((70) "X_PolyFillRectangle")             
    ((71) "X_PolyFillArc")       
    ((72) "X_PutImage")    
    ((73) "X_GetImage") 
    ((74) "X_PolyText8")     
    ((75) "X_PolyText16")      
    ((76) "X_ImageText8")      
    ((77) "X_ImageText16")       
    ((78) "X_CreateColormap")          
    ((79) "X_FreeColormap")        
    ((80) "X_CopyColormapAndFree")               
    ((81) "X_InstallColormap")           
    ((82) "X_UninstallColormap")             
    ((83) "X_ListInstalledColormaps")                  
    ((84) "X_AllocColor")      
    ((85) "X_AllocNamedColor")           
    ((86) "X_AllocColorCells")           
    ((87) "X_AllocColorPlanes")            
    ((88) "X_FreeColors")      
    ((89) "X_StoreColors")       
    ((90) "X_StoreNamedColor")           
    ((91) "X_QueryColors")       
    ((92) "X_LookupColor")       
    ((93) "X_CreateCursor")        
    ((94) "X_CreateGlyphCursor")             
    ((95) "X_FreeCursor")      
    ((96) "X_RecolorCursor")         
    ((97) "X_QueryBestSize")         
    ((98) "X_QueryExtension")          
    ((99) "X_ListExtensions")          
    ((100) "X_ChangeKeyboardMapping")
    ((101) "X_GetKeyboardMapping")
    ((102) "X_ChangeKeyboardControl")                
    ((103) "X_GetKeyboardControl")             
    ((104) "X_Bell")
    ((105) "X_ChangePointerControl")
    ((106) "X_GetPointerControl")
    ((107) "X_SetScreenSaver")
    ((108) "X_GetScreenSaver")          
    ((109) "X_ChangeHosts")       
    ((110) "X_ListHosts")     
    ((111) "X_SetAccessControl")               
    ((112) "X_SetCloseDownMode")
    ((113) "X_KillClient")
    ((114) "X_RotateProperties")
    ((115) "X_ForceScreenSaver")
    ((116) "X_SetPointerMapping")
    ((117) "X_GetPointerMapping")
    ((118) "X_SetModifierMapping")
    ((119) "X_GetModifierMapping")
    ((127) "X_NoOperation")
    (else "unknown")))

(define (x-error->string e)
  (string-append (x-error:text e) "\n"
		 "  Major Opcode:  " (number->string (x-error:major-opcode e))
		 " (" (opcode->string (x-error:major-opcode e)) ")\n"
		 "  Resource ID:   " (number->string (x-error:resource-id e))))

(define-condition-type 'x-warning '(warning))
(define x-warning? (condition-predicate 'x-warning))
(define (x-warning:x-error w)
  (cadr (condition-stuff w)))
(define (signal-x-warning x-error)
  (signal 'x-warning (x-error:text x-error)
	  (opcode->string (x-error:major-opcode x-error))
	  (x-error:resource-id x-error)
	  x-error))

;; Call synchronize to have the warnings signaled where they belong to.

(define (use-x-error-warnings! display on?)
  (let ((was (display:warnings? display))) ;; lock??
    (set-display:warnings?! display on?)
    was))

;; *** error-queue ***************************************************

;; Interface:
;; (empty-x-error-queue? q) return #t only for the initial queue.
;; (next-x-error-queue q) returns the next queue element, blocks if necessary.
;; (x-error-queue:this q) returns the x-error of that queue.
	    
(define-record-type x-error-queue :x-error-queue
  (really-make-x-error-queue this next)
  x-error-queue?
  (this x-error-queue:this)
  (next really-next-x-error-queue really-set-next-x-error-queue!))

(define (make-x-error-queue error)
  (really-make-x-error-queue error (make-placeholder)))

(define (empty-x-error-queue)
  (make-x-error-queue #f))

(define (empty-x-error-queue? obj)
  (eq? obj empty-x-error-queue))

(define (next-x-error-queue x-error-queue)
  (placeholder-value (really-next-x-error-queue x-error-queue)))

(define (set-next-x-error-queue! x-error-queue next-x-error-queue)
  (placeholder-set! (really-next-x-error-queue x-error-queue)
		    next-x-error-queue))

;; *** default error handlers ****************************************

(define (internal-x-error-handler display error)
  (let ((queue (make-x-error-queue error)))
    (set-next-x-error-queue! (display:error-queue display) queue)
    (set-display:error-queue! display queue)))

(define-exported-binding "internal-x-error-handler" internal-x-error-handler)

(import-lambda-definition get-error-text (display code)
  "scx_Get_Error_Text")

(import-lambda-definition get-error-database-text
  (display name message default-string)
  "scx_Get_Error_Database_Text")

;(import-lambda-definition %set-io-error-handler (handler)
;  "scx_Set_IO_Error_Handler")

(define *x-fatal-error-handler* #f)
(define (internal-x-fatal-error-handler display)
  (if *x-fatal-error-handler*
      (*x-fatal-error-handler* display)
      #f))
(define-exported-binding "internal-x-fatal-error-handler"
  internal-x-fatal-error-handler)

(define (set-fatal-error-handler! handler)
  (let ((old-handler *x-fatal-error-handler*))
    (set! *x-fatal-error-handler* handler)
    old-handler))
