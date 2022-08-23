(in-package :clim-sdl2)

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; When in doubt we use "round half up" rounding, instead of the CL:ROUND
  ;; "round half to even".
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often want to
  ;; operate with coordinates, which are multiples of 1/2.  Using CL:ROUND gives
  ;; "random" results. Using "round half up" gives you more consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners, while in X11
  ;; they are at the centers. We don't do much about the discrepancy, but
  ;; rounding up at half pixel boundaries seems to work well.
  (etypecase x
    (integer      x)
    (single-float (values (floor (+ x .5f0))))
    (double-float (values (floor (+ x .5d0))))
    (long-float   (values (floor (+ x .5l0))))
    (ratio        (values (floor (+ x 1/2))))))
