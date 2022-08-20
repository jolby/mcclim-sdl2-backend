;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-sdl2
  (:use :clim :clim-lisp :clim-backend)
  (:import-from #:climi #:maybe-funcall)
  (:local-nicknames
   (#:alx #:alexandria)))
