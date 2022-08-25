(cl:in-package #:asdf-user)

(defsystem #:mcclim-sdl2
  :depends-on ("mcclim-backend-common" "mcclim-fonts" "mcclim/extensions"
                                       "sdl2" "log4cl" "trivial-main-thread")
  :serial t
  :components ((:module "jackdaniel-spec"
                :pathname "src"
                :components ((:file "package")
                             (:file "class-defs" :depends-on ("package"))
                             (:file "basic" :depends-on ("package" "class-defs"))
                             (:file "port" :depends-on ("package" "class-defs" "basic"))
                             (:file "mirror" :depends-on ("package" "class-defs" "basic" "port"))
                             (:file "frame-manager" :depends-on ("package" "class-defs" "basic" "port"))
                             ;; (:file "medium" :depends-on ("port" "package"))
                             ;; (:file "graft" :depends-on ("port" "package"))

                             )))

  ;; :components ((:module "sdl-test-branch"
  ;;               :components ((:file "package")
  ;;                            (:file "basic" :depends-on ("package"))
  ;;                            (:file "port" :depends-on ("basic"))
  ;;                            (:file "medium" :depends-on ("port" "package"))
  ;;                            (:file "graft" :depends-on ("port" "package"))
  ;;                            (:file "frame-manager" :depends-on ("medium" "port" "package")))))

  )

#+nil (asdf:load-system :mcclim-sdl2)

#+nil (in-package :mcclim-sdl2)
