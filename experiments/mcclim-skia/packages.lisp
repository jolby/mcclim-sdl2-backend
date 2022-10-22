;;; Package definition.

(defpackage #:skia-core
  (:use :cl)
  (:local-nicknames (#:alx #:alexandria)
                    (#:sv #:static-vectors))
  (:export
   #:check-false
   #:check-nil
   #:check-nullptr
   #:check-nil-or-nullptr
   #:make-skia-context
   #:destroy-skia-context))

(defpackage #:skia-canvas
  (:use :cl #:skia-core)
  (:local-nicknames (#:alx #:alexandria)
                    (#:sv #:static-vectors))
  (:import-from #:skia-core
                #:check-false
                #:check-nil
                #:check-nullptr
                #:check-nil-or-nullptr
                ))

(defpackage #:mcclim-skia
  (:use #:clim-lisp #:clim #:clime #:climb :mcclim-sdl2)
  (:local-nicknames (#:alx #:alexandria)
                    (#:tch #:lparallel.queue)
                    (#:canvas #:skia-canvas)))

(in-package #:mcclim-skia)
