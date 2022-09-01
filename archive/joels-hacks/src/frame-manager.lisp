;;; -*- Mode: Lisp; Package: CLIM-SDL2 -*-


(in-package :clim-sdl2)
;;; We use &ALLOW-OTHER-KEYS since the INITIALIZE-INSTANCE for
;;; SDL-PORT passes various initargs that SDL-FRAME-MANAGER doesn't
;;; necessarily accept.
(defmethod initialize-instance :after ((instance sdl2-frame-manager)
                                       &key &allow-other-keys))

(defmethod adopt-frame :after ((fm sdl2-frame-manager) (frame application-frame))
  (log:info "fm=~s frame=~s" fm frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    nil))

(defmethod note-space-requirements-changed :after ((graft sdl2-graft) pane)
  ())


(defmethod find-concrete-pane-class ((fm sdl2-frame-manager) pane-type &optional errorp)
  (if (eq pane-type 'climi::top-level-sheet-pane)
      (find-class 'sdl2-top-level-sheet-pane)
      (call-next-method)))
