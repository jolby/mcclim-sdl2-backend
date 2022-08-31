;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-sdl2
  (:use :clim :clim-lisp :clim-backend :clim-internals)
  (:import-from #:climi
                #:if-let
                #:when-let
                #:maybe-funcall
                #:sheet-mirror-geometry
                #:sheet-pretty-name)
  (:local-nicknames
   (#:alx #:alexandria)))
