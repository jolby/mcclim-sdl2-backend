(in-package :clim-sdl2-demo/002-create-plain-sheet)

(defvar *current-port* nil)
(defvar *plain-sheet* nil)

(defvar *glider*
  (clim:make-pattern-from-bitmap-file
   (asdf:component-pathname
    (asdf:find-component "clim-examples" '("images" "glider.png")))))

(defclass plain-sheet (;; repainting
                       clim:immediate-repainting-mixin
                       ;; input
                       clim:immediate-sheet-input-mixin
                       ;; output
                       clim:sheet-mute-output-mixin
                       ;; geometry
                       clim:sheet-identity-transformation-mixin
                       ;; genealogy
                       clim:sheet-parent-mixin
                       clim:sheet-leaf-mixin
                       ;; windowing
                       clime:top-level-sheet-mixin
                       clim:mirrored-sheet-mixin
                       ;; the base class
                       clim:basic-sheet)
  ()
  (:default-initargs :icon *glider*
                     :pretty-name "McCLIM Test Sheet"
                     :region (clim:make-rectangle* 1000 100 1400 500)))

(defun ensure-current-port ()
  (unless *current-port*
    (let ((sdl2-port (find-port :server-path :sdl2)))
      (setf *current-port* sdl2-port)))
  *current-port*)

(defun start-port-event-loop ()
  (let ((port (ensure-current-port)))
    (clim-sdl2::run-port-loop-in-main-thread port)))

(defun stop-port-event-loop ()
  (clim-sdl2::sdl2-exit-port)
  ;;Alternative:
  ;;(setf clim-sdl2::*keep-running* nil)
  )

(defun create-plain-sheet-demo ()
  (unless *current-port*
    (error "No current sdl2-port. Use ensure-current-port before calling this."))
  (unless clim-sdl2::*is-running*
    (error "No current sdl2-port loop running. Use start-port-event-loop before calling this."))

  (setf *plain-sheet* (make-instance 'plain-sheet))
  (log:info "Created plain sheet: ~a" *plain-sheet*)

  ;; This does create an SDL window.
  (realize-mirror *current-port* *plain-sheet*)


  ;; This sets the slot, but errors afterwards
  ;; There is no applicable method for the generic function
  ;; #<STANDARD-GENERIC-FUNCTION CLIM-BACKEND:PORT-SET-MIRROR-NAME (2)>
  ;; when called with arguments (NIL
  ;; #<CLIM-SDL2-DEMO/002-CREATE-PLAIN-SHEET::PLAIN-SHEET {100A498543}>
  ;; "BLAH BLAH BLAH").
  (setf (climi::sheet-pretty-name *plain-sheet*) "BLAH BLAH BLAH")

  ;; *plain-sheet* doesn't have the port slot set. Does that need to be
  ;; set in realize-mirror??? CLX doesn't appear to do that. The old
  ;; sdl-test branch made a call: (climi:port-register-mirror port sheet mirror)
  ;; that doesn't exist anymore
  (destroy-mirror (port *plain-sheet*) *plain-sheet*)
  )

(defun run ()
  (progn
    (start-port-event-loop)
    ;; XXX should probably have an is-running condition var
    ;; on port???
    (sleep 1)
    (create-plain-sheet-demo)
   ))
