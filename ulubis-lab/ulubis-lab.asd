

(asdf:defsystem :ulubis-lab
  :description "libwayland client for Common Lisp"
  :author "Earl DuCaine"
  :license "MIT"
  :depends-on (:cffi :closer-mop :xmls :cl-wayland-client)
  :defsystem-depends-on (:cffi-grovel)
  :serial t
  :components ((:file "package")
	       (:file "simple-touch")
	       (:file "ulubis-lab-utils")
	       (:file "ulubis-lab-def")
	       ;; (:cffi-wrapper-file "wrappers" :soname "libulubislab")
	       (:file "ulubis-lab")))
