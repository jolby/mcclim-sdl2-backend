;;; Package definition.

(defpackage #:skia-core
  (:use :cl)
  (:local-nicknames (#:alx #:alexandria))
  (:export #:make-skia-context
           #:destroy-skia-context))

(defpackage #:skia-canvas
  (:use :cl #:skia-core)
  (:local-nicknames (#:alx #:alexandria)))

(defpackage #:mcclim-skia
  (:use #:clim-lisp #:clim #:clime #:climb :mcclim-sdl2)
  (:local-nicknames (#:alx #:alexandria)
                    (#:tch #:lparallel.queue)
                    (#:canvas #:skia-canvas)))

(in-package #:mcclim-skia)
