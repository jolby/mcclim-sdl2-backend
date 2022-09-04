;;; Package definition.

(defpackage #:mcclim-skia
  (:use #:clim-lisp #:clim #:clime #:climb :mcclim-sdl2)
  (:local-nicknames (#:alx #:alexandria)
                    (#:tch #:lparallel.queue)))
(in-package #:mcclim-skia)
