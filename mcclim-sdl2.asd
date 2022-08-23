(cl:in-package #:asdf-user)

(defsystem #:mcclim-sdl2
  :depends-on ("mcclim-backend-common" "mcclim-fonts" "mcclim/extensions" "sdl2" "cl-cairo2" "log4cl")
  :serial t
  :components ((:module "jackdaniel-spec"
                :pathname "src"
                :components ((:file "package")
                             (:file "basic" :depends-on ("package"))
                             (:file "utilities" :depends-on ("package"))
                             (:file "port" :depends-on ("basic"))
                             (:file "mirror" :depends-on ("basic" "port"))
                             ;; (:file "medium" :depends-on ("port" "package"))
                             ;; (:file "graft" :depends-on ("port" "package"))
                             ;; (:file "frame-manager" :depends-on ("medium" "port" "package"))

                             )))

  ;; :components ((:module "sdl-test-branch"
  ;;               :components ((:file "package")
  ;;                            (:file "basic" :depends-on ("package"))
  ;;                            (:file "port" :depends-on ("basic"))
  ;;                            (:file "medium" :depends-on ("port" "package"))
  ;;                            (:file "graft" :depends-on ("port" "package"))
  ;;                            (:file "frame-manager" :depends-on ("medium" "port" "package")))))

  )
