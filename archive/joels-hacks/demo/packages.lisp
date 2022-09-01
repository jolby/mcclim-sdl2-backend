;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-sdl2-demo/001-port-event-loop
  (:use :clim :clim-lisp :clim-internals)
  (:local-nicknames
   (#:alx #:alexandria))
  (:export #:run))

(defpackage :clim-sdl2-demo/002-create-plain-sheet
  (:use :clim :clim-lisp :clim-internals)
  (:local-nicknames
   (#:alx #:alexandria))
  (:export #:run))
