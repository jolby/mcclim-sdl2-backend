(in-package #:mcclim-sdl2)

(defparameter mod-mappings (list (cons sdl2-ffi::+kmod-lalt+ :ALT-LEFT)
                                 (cons sdl2-ffi::+kmod-lctrl+ :CONTROL-LEFT)
                                 (cons sdl2-ffi::+kmod-lgui+ :SUPER-LEFT)
                                 (cons sdl2-ffi::+kmod-lshift+ :SHIFT-LEFT)
                                 (cons sdl2-ffi::+kmod-ralt+ :ALT-RIGHT)
                                 (cons sdl2-ffi::+kmod-rctrl+ :CONTROL-RIGHT)
                                 (cons sdl2-ffi::+kmod-rgui+ :SUPER-RIGHT)
                                 (cons sdl2-ffi::+kmod-rshift+ :SHIFT-RIGHT)))

(defparameter *keysm->modifier*
  `((:ALT-LEFT ,+meta-key+)
    (:ALT-RIGHT ,+meta-key+)
    (:CONTROL-LEFT ,+control-key+)
    (:CONTROL-RIGHT ,+control-key+)
    (:SHIFT-LEFT ,+shift-key+)
    (:SHIFT-RIGHT ,+shift-key+)
    (:SUPER-LEFT ,+super-key+)
    (:SUPER-RIGHT ,+hyper-key+)))

(defstruct (sdl2-keyevent-info
            ;; (:constructor make-keyevent-info ())
            (:conc-name %keyevent-))
  (scancode nil)
  (scancode-keyword nil)
  (scancode-name nil)
  (key-code nil)
  (key-code-keyword nil)
  (key-code-name nil))

;;These are missing from cl-sdl2 keyboard.lisp
(defun %key-code-keyword (key-code)
  (autowrap:enum-key 'sdl2-ffi:sdl-key-code key-code))

(defun %scancode-from-key-code (key-code)
  (alx:when-let ((scancode (sdl2-ffi.functions:sdl-get-scancode-from-key key-code)))
    (when (plusp scancode) scancode)))

(defun %scancode->vals (sdl2-scancode)
  (let* ((scancode sdl2-scancode)
         (scancode-keyword (sdl2:scancode-symbol scancode))
         (scancode-name (sdl2:scancode-name scancode))
         (key-code (sdl2:get-key-from-scancode scancode))
         (key-code-keyword (%key-code-keyword key-code))
         (key-code-name (sdl2:scancode-key-name scancode))
         )
    (values scancode key-code
            scancode-keyword key-code-keyword
            scancode-name key-code-name)))

(defun %scancode->kw-vals (sdl2-scancode)
  (let* ((scancode sdl2-scancode)
         (scancode-keyword (sdl2:scancode-symbol scancode))
         (scancode-name (sdl2:scancode-name scancode))
         (key-code (sdl2:get-key-from-scancode scancode))
         (key-code-keyword (%key-code-keyword key-code))
         (key-code-name (sdl2:scancode-key-name scancode))
         )
    (values :scancode scancode :key-code key-code
            :scancode-keyword scancode-keyword :key-code-keyword key-code-keyword
            :scancode-name scancode-name :key-code-name key-code-name)))

(defun %key-code->vals (key-code)
  (alx:when-let ((scancode (%scancode-from-key-code key-code)))
    (%scancode->vals scancode)))

(defun make-sdl2-keyevent-info-from-scancode (sdl2-scancode)
  (multiple-value-call #'make-sdl2-keyevent-info (%scancode->kw-vals scancode)))

(defun make-sdl2-keyevent-info-from-key-code (sdl2-key-code)
  (alx:if-let ((scancode-value (%scan-code-from-key-code)))
    (multiple-value-call #'make-sdl2-keyevent-info (%scancode->kw-vals scancode-value))))

(defun make-sdl2-keyevent-info-from-event (sdl2-keysym-event)
  (make-sdl2-keysym-info-from-scancode (sdl2:scancode-value sdl2-keysym-event)))

  ;;Mapping from SDL2 keycode modifier Keyword to integer value
(defparameter *sdl2-keymod-map*
  (alx:alist-hash-table
   (autowrap:foreign-enum-values
    (autowrap::require-type 'sdl2-ffi:sdl-keymod
                            "Unable to load enum: ~a" 'sdl2-ffi:sdl-keymod))))

;;Mapping from SDL2 scancode keyword to integer value
;;https://wiki.libsdl.org/SDL_Scancode
(defparameter *sdl2-scancode-map*
  (alx:alist-hash-table
   (autowrap:foreign-enum-values
    (autowrap::require-type 'sdl2-ffi:sdl-scancode
                            "Unable to load enum: ~a" 'sdl2-ffi:sdl-scancode))))

;;Mapping from SDL2 key-code keyword to integer value
;;https://wiki.libsdl.org/SDL_Keycode
(defparameter *sdl2-key-code-map*
  (alx:alist-hash-table
   (autowrap:foreign-enum-values
    (autowrap::require-type 'sdl2-ffi:sdl-key-code
                            "Unable to load enum: ~a" 'sdl2-ffi:sdl-key-code))))

(defvar *keysym-name-table*
  (make-hash-table :test #'eql))

;;; This hash table maps a keysym name to the corresponding keysym.
(defvar *keysym-table*
  (make-hash-table :test #'eq))

(defun map-keysym->modifier (keysym)
  (cdr (assoc keysym *keysm->modifier*)))

(defun decode-sdl2-mod-state (state)
  (let ((mods 0))
    (dolist (mod mod-mappings)
      (when (> (logand state (car mod)) 0)
        (let ((m (map-keysym->modifier (cdr mod))))
          (when m
            (setf mods (logior (car m) mods))))))
    mods))

(defun decode-sdl2-mod-state-keywords (state)
  (let ((mods ()))
    (dolist (mod mod-mappings)
      (when (> (logand state (car mod)) 0)
        (pushnew (cdr mod) mods)))
    mods))

(defun define-keysym (name value &optional (alpha-p nil))
  (pushnew (list name alpha-p) (gethash value *keysym-name-table* nil))
  (setf (gethash name *keysym-table*) value))

(defun keysym-to-keysym-name (value)
  (values-list (car (last (gethash value *keysym-name-table*)))))

(defun keysym-name-to-keysym (value)
  (gethash value *keysym-table*))

(defun sdl2-event-to-key-name-and-modifiers (code state)
  (let ((shift? (logtest (logior sdl2-ffi::+kmod-lshift+ sdl2-ffi::+kmod-rshift+) state)))
    (multiple-value-bind (keysym-name alpha-p)
        (keysym-to-keysym-name code)
      (let ((char
              (and (symbolp keysym-name)
                   (< code 255)
                   (code-char code))))
        (values
         char
         alpha-p
         (decode-sdl2-mod-state state)
         keysym-name)))))

(define-keysym :|1| sdl2-ffi::+SDLK-1+ t)
(define-keysym :|2| sdl2-ffi::+SDLK-2+ t)
(define-keysym :|3| sdl2-ffi::+SDLK-3+ t)
(define-keysym :|4| sdl2-ffi::+SDLK-4+ t)
(define-keysym :|5| sdl2-ffi::+SDLK-5+ t)
(define-keysym :|6| sdl2-ffi::+SDLK-6+ t)
(define-keysym :|7| sdl2-ffi::+SDLK-7+ t)
(define-keysym :|8| sdl2-ffi::+SDLK-8+ t)
(define-keysym :|9| sdl2-ffi::+SDLK-9+ t)
(define-keysym :|0| sdl2-ffi::+SDLK-0+ t)
(define-keysym :|`| sdl2-ffi::+SDLK-BACKQUOTE+ t)
(define-keysym :|'| sdl2-ffi::+SDLK-QUOTE+ t)
(define-keysym :|"| sdl2-ffi::+SDLK-QUOTEDBL+ t)
(define-keysym :|!| sdl2-ffi::+SDLK-EXCLAIM+ t)
(define-keysym :|@| sdl2-ffi::+SDLK-AT+ t)
(define-keysym :|#| sdl2-ffi::+SDLK-HASH+ t)
(define-keysym :|$| sdl2-ffi::+SDLK-DOLLAR+ t)
(define-keysym :|%| sdl2-ffi::+SDLK-PERCENT+ t)
(define-keysym :|^| sdl2-ffi::+SDLK-CARET+ t)
(define-keysym :|&| sdl2-ffi::+SDLK-AMPERSAND+ t)
(define-keysym :|*| sdl2-ffi::+SDLK-ASTERISK+ t)
(define-keysym :|(| sdl2-ffi::+SDLK-LEFTPAREN+ t)
(define-keysym :|)| sdl2-ffi::+SDLK-RIGHTPAREN+ t)
(define-keysym :|+| sdl2-ffi::+SDLK-PLUS+ t)
(define-keysym :|,| sdl2-ffi::+SDLK-COMMA+ t)
(define-keysym :|-| sdl2-ffi::+SDLK-MINUS+ t)
(define-keysym :|.| sdl2-ffi::+SDLK-PERIOD+ t)
(define-keysym :|/| sdl2-ffi::+SDLK-SLASH+ t)
(define-keysym :|:| sdl2-ffi::+SDLK-COLON+ t)
(define-keysym :|;| sdl2-ffi::+SDLK-SEMICOLON+ t)
(define-keysym :|=| sdl2-ffi::+SDLK-EQUALS+ t)
(define-keysym :|>| sdl2-ffi::+SDLK-GREATER+ t)
(define-keysym :|<| sdl2-ffi::+SDLK-LESS+ t)
(define-keysym :|?| sdl2-ffi::+SDLK-QUESTION+ t)
(define-keysym :|[| sdl2-ffi::+SDLK-LEFTBRACKET+ t)
(define-keysym :|]| sdl2-ffi::+SDLK-RIGHTBRACKET+ t)
(define-keysym :|\\| sdl2-ffi::+SDLK-BACKSLASH+ t)
(define-keysym :|_| sdl2-ffi::+SDLK-UNDERSCORE+ t)
(define-keysym :| | sdl2-ffi::+SDLK-SPACE+ t)
(define-keysym :|a| sdl2-ffi::+SDLK-A+ t)
(define-keysym :|b| sdl2-ffi::+SDLK-B+ t)
(define-keysym :|c| sdl2-ffi::+SDLK-C+ t)
(define-keysym :|d| sdl2-ffi::+SDLK-D+ t)
(define-keysym :|e| sdl2-ffi::+SDLK-E+ t)
(define-keysym :|f| sdl2-ffi::+SDLK-F+ t)
(define-keysym :|g| sdl2-ffi::+SDLK-G+ t)
(define-keysym :|h| sdl2-ffi::+SDLK-H+ t)
(define-keysym :|i| sdl2-ffi::+SDLK-I+ t)
(define-keysym :|j| sdl2-ffi::+SDLK-J+ t)
(define-keysym :|k| sdl2-ffi::+SDLK-K+ t)
(define-keysym :|l| sdl2-ffi::+SDLK-L+ t)
(define-keysym :|m| sdl2-ffi::+SDLK-M+ t)
(define-keysym :|n| sdl2-ffi::+SDLK-N+ t)
(define-keysym :|o| sdl2-ffi::+SDLK-O+ t)
(define-keysym :|p| sdl2-ffi::+SDLK-P+ t)
(define-keysym :|q| sdl2-ffi::+SDLK-Q+ t)
(define-keysym :|r| sdl2-ffi::+SDLK-R+ t)
(define-keysym :|s| sdl2-ffi::+SDLK-S+ t)
(define-keysym :|t| sdl2-ffi::+SDLK-T+ t)
(define-keysym :|u| sdl2-ffi::+SDLK-U+ t)
(define-keysym :|v| sdl2-ffi::+SDLK-V+ t)
(define-keysym :|w| sdl2-ffi::+SDLK-W+ t)
(define-keysym :|x| sdl2-ffi::+SDLK-X+ t)
(define-keysym :|y| sdl2-ffi::+SDLK-Y+ t)
(define-keysym :|z| sdl2-ffi::+SDLK-Z+ t)

(define-keysym :BACKSPACE sdl2-ffi::+SDLK-BACKSPACE+)
(define-keysym :TAB sdl2-ffi::+SDLK-TAB+)
(define-keysym :RETURN sdl2-ffi::+SDLK-RETURN+)
(define-keysym :ESCAPE sdl2-ffi::+SDLK-ESCAPE+)
(define-keysym :DELETE sdl2-ffi::+SDLK-DELETE+)

(define-keysym :SHIFT-LEFT sdl2-ffi::+SDLK-LSHIFT+)
(define-keysym :SHIFT-RIGHT sdl2-ffi::+SDLK-RSHIFT+)
(define-keysym :CONTROL-LEFT sdl2-ffi::+SDLK-LCTRL+)
(define-keysym :CONTROL-RIGHT sdl2-ffi::+SDLK-RCTRL+)
(define-keysym :CAPS-LOCK sdl2-ffi::+SDLK-CAPSLOCK+)
(define-keysym :MENU sdl2-ffi::+SDLK-MENU+)
(define-keysym :ALT-LEFT sdl2-ffi::+SDLK-LALT+)
(define-keysym :ALT-RIGHT sdl2-ffi::+SDLK-RALT+)
(define-keysym :SUPER-LEFT sdl2-ffi::+SDLK-LGUI+)
(define-keysym :HOME sdl2-ffi::+SDLK-HOME+)
(define-keysym :LEFT sdl2-ffi::+SDLK-LEFT+)
(define-keysym :UP sdl2-ffi::+SDLK-UP+)
(define-keysym :RIGHT sdl2-ffi::+SDLK-RIGHT+)
(define-keysym :DOWN sdl2-ffi::+SDLK-DOWN+)
(define-keysym :PRIOR sdl2-ffi::+SDLK-PRIOR+)
(define-keysym :PRIOR sdl2-ffi::+SDLK-PAGEUP+)
(define-keysym :NEXT sdl2-ffi::+SDLK-PAGEDOWN+)
(define-keysym :END sdl2-ffi::+SDLK-END+)
(define-keysym :SELECT sdl2-ffi::+SDLK-SELECT+)
(define-keysym :PRINT sdl2-ffi::+SDLK-PRINTSCREEN+)
(define-keysym :EXECUTE sdl2-ffi::+SDLK-EXECUTE+)
(define-keysym :INSERT sdl2-ffi::+SDLK-INSERT+)
(define-keysym :UNDO sdl2-ffi::+SDLK-UNDO+)
(define-keysym :FIND sdl2-ffi::+SDLK-FIND+)
(define-keysym :CANCEL sdl2-ffi::+SDLK-CANCEL+)
(define-keysym :HELP sdl2-ffi::+SDLK-HELP+)
(define-keysym :PAUSE sdl2-ffi::+SDLK-PAUSE+)
(define-keysym :SCROLL-LOCK sdl2-ffi::+SDLK-SCROLLLOCK+)

(define-keysym :F1 sdl2-ffi::+SDLK-F1+)
(define-keysym :F2 sdl2-ffi::+SDLK-F2+)
(define-keysym :F3 sdl2-ffi::+SDLK-F3+)
(define-keysym :F4 sdl2-ffi::+SDLK-F4+)
(define-keysym :F5 sdl2-ffi::+SDLK-F5+)
(define-keysym :F6 sdl2-ffi::+SDLK-F6+)
(define-keysym :F7 sdl2-ffi::+SDLK-F7+)
(define-keysym :F8 sdl2-ffi::+SDLK-F8+)
(define-keysym :F9 sdl2-ffi::+SDLK-F9+)
(define-keysym :F10 sdl2-ffi::+SDLK-F10+)
(define-keysym :F11 sdl2-ffi::+SDLK-F11+)
(define-keysym :F12 sdl2-ffi::+SDLK-F12+)
(define-keysym :KP-ENTER sdl2-ffi::+SDLK-KP-ENTER+)
(define-keysym :KP-MULTIPLY sdl2-ffi::+SDLK-KP-MEMMULTIPLY+ t)
(define-keysym :KP-ADD sdl2-ffi::+SDLK-KP-PLUS+ t)
(define-keysym :KP-SEPARATOR sdl2-ffi::+SDLK-KP-PERIOD+ t)
(define-keysym :KP-SUBTRACT sdl2-ffi::+SDLK-KP-MINUS+ t)
(define-keysym :KP-DECIMAL sdl2-ffi::+SDLK-KP-PERIOD+ t)
(define-keysym :KP-DIVIDE sdl2-ffi::+SDLK-KP-DIVIDE+ t)
(define-keysym :KP-0 sdl2-ffi::+SDLK-KP-0+ t)
(define-keysym :KP-1 sdl2-ffi::+SDLK-KP-1+ t)
(define-keysym :KP-2 sdl2-ffi::+SDLK-KP-2+ t)
(define-keysym :KP-3 sdl2-ffi::+SDLK-KP-3+ t)
(define-keysym :KP-4 sdl2-ffi::+SDLK-KP-4+ t)
(define-keysym :KP-5 sdl2-ffi::+SDLK-KP-5+ t)
(define-keysym :KP-6 sdl2-ffi::+SDLK-KP-6+ t)
(define-keysym :KP-7 sdl2-ffi::+SDLK-KP-7+ t)
(define-keysym :KP-8 sdl2-ffi::+SDLK-KP-8+ t)
(define-keysym :KP-9 sdl2-ffi::+SDLK-KP-9+ t)
