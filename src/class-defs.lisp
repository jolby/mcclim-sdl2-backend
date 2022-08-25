(in-package :clim-sdl2)

;; Conditions
(define-condition sdl2-exit-port (condition) ())


;; Classes
(defclass sdl2-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass sdl2-port (basic-port)
  ((id)
   (pointer            :accessor port-pointer :initform (make-instance 'sdl2-pointer))
   (window             :initform nil :accessor sdl-port-window)
   (sheet-to-window-id :initform (make-hash-table :test 'eql)
                       :reader sdl-port/sheet-to-window-id)
   (window-id-to-sheet :initform (make-hash-table :test 'eql)
                       :reader sdl-port/window-id-to-sheet)))

(defclass sdl2-mirror ()
  ((window
    :initarg :window
    :reader window))
  (:default-initargs :window (alexandria:required-argument :window)))

(defclass sdl2-graft (graft)
  ())

(defclass sdl-renderer-sheet (mirrored-sheet-mixin)
  ((renderer      :initform nil
                  :accessor sdl-renderer-sheet/renderer)
   (texture       :initform nil
                  :accessor sdl-renderer-sheet/texture)
   (surface       :initform nil
                  :accessor sdl-renderer-sheet/surface)
   (drawing-context :initform nil
                  :accessor sdl-renderer-sheet/drawing-context)))

(defclass sdl2-top-level-sheet-pane (sdl2-renderer-sheet climi::top-level-sheet-pane)
  ())

(defmethod sheet-direct-mirror ((sheet sdl2-top-level-sheet-pane))
  ())

(defclass sdl2-frame-manager (frame-manager)
  ())
