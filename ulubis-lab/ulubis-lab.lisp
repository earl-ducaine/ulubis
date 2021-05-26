


(in-package :ulubis-lab)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (pushnew (directory-namestring
	    (merge-pathnames
	     (directory-namestring
	      (asdf:system-source-directory :ulubis-lab))))
	   *foreign-library-directories*
	   :test #'string=))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (define-foreign-library ulubis-lab-simple-egl
  (t (:default
      "libulubis-lab-simple-egl")))

  (use-foreign-library ulubis-lab-simple-egl))


(defparameter *swap-damage-ext-to-entrypoint*
  (let* ((type '(:struct swap-damage-ext-to-entrypoint))
         (swap-damage-ext-to-entrypoint (foreign-alloc type :count 2)))
    (mapcar
     (lambda (entrypoint)
       (let ((ptr (mem-aptr swap-damage-ext-to-entrypoint type
			    (caddr entrypoint))))
	 (setf (foreign-slot-value ptr type 'extension)
	       (car entrypoint))
	 (setf (foreign-slot-value ptr type 'entrypoint)
	       (cadr entrypoint))))
     '(("EGL-EXT-swap-buffers-with-damage"
	"eglSwapBuffersWithDamageEXT" 0)
       ("EGL-KHR-swap-buffers-with-damage"
	"eglSwapBuffersWithDamageKHR" 1)))
    swap-damage-ext-to-entrypoint))

(defcfun "app_main" :int
  (argc :int)
  (argv :pointer)
  (window-ptr (:pointer (:struct window)))
  (display-ptr (:pointer (:struct display))))


(defclass cffi-pointer-wrapper ()
  ;; sap: system area pointer.
  ((sap :accessor sap :initarg :sap)
   (cffi-type :accessor cffi-type :initarg :cffi-type)))

(defclass window (cffi-pointer-wrapper)
  ())

(defclass display (cffi-pointer-wrapper)
  ())

(defmethod -> ((window-ptr window) slot)
  (with-slots (sap cffi-type) window-ptr
    (foreign-slot-value sap  '(:struct window) slot)))

(defmethod (setf ->) ((value cffi-pointer-wrapper) (window-ptr window) slot)
  (with-slots (sap cffi-type) window-ptr
    (setf (foreign-slot-value sap cffi-type slot) (sap value))))

(defmethod -> ((display-ptr display) slot)
  (with-slots (sap cffi-type) display-ptr
    (foreign-slot-value sap '(:struct display) slot)))

(defmethod (setf ->) (value (display-ptr display) slot)
  (with-slots (sap cffi-type) display-ptr
    (setf (foreign-slot-value sap cffi-type slot) value)))

(defun test-window-ptr ()
  (with-foreign-objects ((window-ptr-raw '(:struct window) 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-ptr-raw
				     :cffi-type '(:struct window))))
      (-> window-ptr 'display))))


(defun run-ulubis-lab ()
  (with-foreign-objects ((window-sap '(:struct window) 1)
			 (display-sap '(:struct display) 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-sap
				     :cffi-type '(:struct window)))
	  (display-ptr (make-instance 'display
				      :sap display-sap
				      :cffi-type '(:struct display))))
      ;; (setf (-> window-ptr 'display) display-ptr)

      ;; window_ptr->display = display_ptr;
      ;; display_ptr->window = window_ptr;
      ;; window_ptr->geometry.width  = 250;
      ;; window_ptr->geometry.height = 250;
      ;; window_ptr->window_size = window_ptr->geometry;
      ;; window_ptr->buffer_size = 32;
      ;; window_ptr->frame_sync = 1;
      ;; window_ptr->delay = 0;
      ;; (format t "window_ptr->display = display_ptr -- ~s~%"
      ;; 	      (-> window-ptr 'display))

      (app-main 0 (null-pointer) window-ptr display-ptr))))
