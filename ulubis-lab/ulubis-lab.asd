

(asdf:defsystem :ulubis-lab
  :description "libwayland client for Common Lisp"
  :author "Earl DuCaine"
  :license "MIT"
  :depends-on (:cffi :closer-mop :xmls :cl-wayland-client)
  :serial t
  :components ((:file "package")
	       (:file "simple-touch")
	       ;; (:file "simple-egl")
	       ))
