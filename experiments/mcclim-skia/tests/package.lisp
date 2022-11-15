(cl:defpackage #:mcclim-skia-tests
  (:use #:clim-lisp #:clim #:clime #:fiveam #:mcclim-skia)
  (:export #:run-tests))

(cl:in-package #:mcclim-skia-tests)

(def-suite :mcclim-skia)

(defun run-tests ()
  (run! :mcclim-skia))
