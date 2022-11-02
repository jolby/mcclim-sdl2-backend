(in-package #:mcclim-sdl2)

(defparameter sdl2-mod-mappings
  `((,sdl2-ffi::+kmod-lalt+ . :ALT-LEFT)
    (,sdl2-ffi::+kmod-lctrl+ . :CONTROL-LEFT)
    (,sdl2-ffi::+kmod-lgui+ . :SUPER-LEFT)
    (,sdl2-ffi::+kmod-lshift+ . :SHIFT-LEFT)
    (,sdl2-ffi::+kmod-ralt+ . :ALT-RIGHT)
    (,sdl2-ffi::+kmod-rctrl+ . :CONTROL-RIGHT)
    (,sdl2-ffi::+kmod-rgui+ . :SUPER-RIGHT)
    (,sdl2-ffi::+kmod-rshift+ . :SHIFT-RIGHT)))

(defparameter modifier-keyword->modifier-key-code
  `((:ALT-LEFT . ,+meta-key+)
    (:ALT-RIGHT . ,+meta-key+)
    (:CONTROL-LEFT . ,+control-key+)
    (:CONTROL-RIGHT . ,+control-key+)
    (:SHIFT-LEFT . ,+shift-key+)
    (:SHIFT-RIGHT . ,+shift-key+)
    (:SUPER-LEFT . ,+super-key+)
    (:SUPER-RIGHT . ,+hyper-key+)))

(defparameter shift-char-map
  `((#\1 . #\!)
    (#\2 . #\@)
    (#\3 . #\#)
    (#\4 . #\$)
    (#\5 . #\%)
    (#\6 . #\^)
    (#\7 . #\&)
    (#\8 . #\*)
    (#\9 . #\()
    (#\0 . #\))
    (#\; . #\:)
    (#\' . #\")
    (#\, . #\<)
    (#\. . #\>)
    (#\/ . #\?)
    ))

;;SDL2 doesn't include these in its keysym maps for some reason...
(defparameter special-shift-char-map
  ;;             shifted-key-code shifted-key-char shifted-key-keyword shifted-key-name
  `((#\` . ,(list 126 #\~ :tilde "TILDE"))
    (#\[ . ,(list 123 #\{ :leftbrace "LEFTBRACE"))
    (#\] . ,(list 123 #\} :rightbrace "RIGHTBRACE"))
    (#\\ . ,(list 123 #\| :verticlebar "VERTICLEBAR"))
    ))

;;Mapping from SDL2 scancode keyword to integer value
;;https://wiki.libsdl.org/SDL_Scancode
(defparameter *sdl2-scancode-map*
  (alexandria:alist-hash-table
   (autowrap:foreign-enum-values
    (autowrap::require-type 'sdl2-ffi:sdl-scancode
                            "Unable to load enum: ~a" 'sdl2-ffi:sdl-scancode))))

;;Mapping from SDL2 key-code keyword to integer value
;;https://wiki.libsdl.org/SDL_Keycode
(defparameter *sdl2-key-code-map*
  (alexandria:alist-hash-table
   (autowrap:foreign-enum-values
    (autowrap::require-type 'sdl2-ffi:sdl-key-code
                            "Unable to load enum: ~a" 'sdl2-ffi:sdl-key-code))))

(defparameter *key-info-db*
  (make-array (hash-table-count *sdl2-scancode-map*)))

(defparameter scancode->key-info
  (make-hash-table :size (hash-table-count *sdl2-scancode-map*)))

(defun %scancode-kw->key-code-kw (sc-kw)
  (let ((kw-str (string sc-kw)))
    (format t "sc-kw: ~a, kw-str: ~a ~%" sc-kw kw-str)
    (when (search "SCANCODE-" kw-str)
      (alexandria:make-keyword (subseq kw-str (length "SCANCODE-"))))))

(defun %make-key-info-entry (sc-num-code sc-keyword kc-num-code kc-keyword)
  (let* ((key-char (when (<= 0 kc-num-code 255) (code-char kc-num-code)))
         (key-name (sdl2-ffi.functions:sdl-get-scancode-name sc-num-code))
         (entry (list :scancode sc-num-code :scancode-keyword sc-keyword
                      :key-code kc-num-code :key-code-keyword kc-keyword
                      :key-char key-char :key-name key-name
                      :shifted-key-code nil
                      :shifted-key-code-keyword nil
                      :shifted-key-char nil
                      :shifted-key-name nil)))
    (cond ((and key-char (<= (char-code #\a) kc-num-code (char-code #\z)))
           (let* ((shifted-key-code (- kc-num-code 32))
                  (shifted-key-char (code-char shifted-key-code)))
             (setf (getf entry :shifted-key-code) shifted-key-code
                   (getf entry :shifted-key-char) shifted-key-char
                   (getf entry :shifted-key-code-keyword) (alexandria:make-keyword (string shifted-key-char))
                   (getf entry :shifted-key-name) (format nil "Capital ~a" shifted-key-char))))
          ((and key-char (assoc key-char shift-char-map))
           (let* ((shifted-key-char (cdr (assoc key-char shift-char-map)))
                  (shifted-key-code (char-code shifted-key-char)))
             (setf (getf entry :shifted-key-code) shifted-key-code
                   (getf entry :shifted-key-char) shifted-key-char
                   (getf entry :shifted-key-code-keyword) (autowrap:enum-key 'sdl2-ffi:sdl-key-code shifted-key-code)
                   (getf entry :shifted-key-name) (sdl2-ffi.functions:sdl-get-key-name shifted-key-code))))
          ((and key-char (assoc key-char special-shift-char-map))
           (destructuring-bind (shifted-key-code shifted-key-char
                                shifted-key-keyword shifted-key-name) (cdr (assoc key-char special-shift-char-map))
             (setf (getf entry :shifted-key-code) shifted-key-code
                   (getf entry :shifted-key-char) shifted-key-char
                   (getf entry :shifted-key-code-keyword) shifted-key-keyword
                   (getf entry :shifted-key-name) shifted-key-name))))
    (setf (gethash sc-num-code scancode->key-info) entry)
    entry))

(defun build-key-info-db ()
  (loop for sc-keyword being the hash-keys of *sdl2-scancode-map*
          using (hash-value sc-num-code)
        for i = 0 then (incf i)
        do (alexandria:when-let* ((kc-keyword (%scancode-kw->key-code-kw sc-keyword))
                                  (kc-num-code (gethash kc-keyword *sdl2-key-code-map*)))
             (setf (aref *key-info-db* i)
                   (%make-key-info-entry sc-num-code sc-keyword kc-num-code kc-keyword)  ))))

;;(build-key-info-db)

(defun modifier-keyword->modifier-key-code (modifier-keyword)
  (cdr (assoc modifier-keyword modifier-keyword->modifier-key-code)))

(defun sdl2-mod-shift-p (sdl2-mod-state)
  (logtest (logior sdl2-ffi::+kmod-lshift+ sdl2-ffi::+kmod-rshift+) sdl2-mod-state))

(defun clim-mod-shift-p (clim-mod-state)
  (logtest +shift-key+ clim-mod-state))

(defun sdl2-mod-state->clim-mod-state (sdl2-mod-state)
  (let ((mods 0))
    (dolist (mod sdl2-mod-mappings)
      (when (> (logand sdl2-mod-state (car mod)) 0)
        (let ((m (modifier-keyword->modifier-key-code (cdr mod))))
          (when m
            (setf mods (logior m mods))))))
    mods))

(defun sdl2-mod-state->clim-mod-state-keywords (sdl2-mod-state)
  (let ((mods ()))
    (dolist (mod sdl2-mod-mappings)
      (when (> (logand sdl2-mod-state (car mod)) 0)
        (pushnew (cdr mod) mods)))
    mods))

(defun key-press-event-values-from-sdl2-scancode (sdl2-scancode-code sdl2-mod-state)
  (let* ((key-info-plist (gethash sdl2-scancode-code scancode->key-info))
         (clim-mod-state (sdl2-mod-state->clim-mod-state sdl2-mod-state)))
    (unless key-info-plist (log:warn "Unknown scancode: ~a" sdl2-scancode-code))
    (log:info "key-info: ~a" key-info-plist)
    (if (clim-mod-shift-p clim-mod-state)
        (values (getf key-info-plist :shifted-key-char)
                (getf key-info-plist :shifted-key-code-keyword)
                clim-mod-state)
        (values (getf key-info-plist :key-char)
                (getf key-info-plist :key-code-keyword)
                clim-mod-state))))
