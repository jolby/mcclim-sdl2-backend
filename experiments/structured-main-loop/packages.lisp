;;; Package definition.

(defpackage #:mcclim-sdl2
  (:use #:clim-lisp #:clim #:clime #:climb)
  (:local-nicknames (#:alx #:alexandria)
                    (#:tch #:lparallel.queue)
                    (#:mt  #:cl-muth)))
(in-package #:mcclim-sdl2)
