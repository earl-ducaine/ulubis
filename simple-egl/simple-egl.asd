

(asdf:defsystem :simple-egl
  :description "libwayland client for Common Lisp"
  :author "Earl Kenct"
  :license "MIT"
  :depends-on (:cffi :closer-mop :xmls :cl-wayland-client)
  :serial t
  :components ((:file "package")
	       (:file "simple-egl-defines")
	       ;; (:file "simple-egl")
	       ))
