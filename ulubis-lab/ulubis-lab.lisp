


(in-package :ulubis-lab)



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



(defun test-window-ptr ()
  (with-foreign-objects ((window-ptr-raw '(:struct window) 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-ptr-raw
				     :cffi-type '(:struct window))))
      (-> window-ptr 'display))))


;; void registry_handle_global(void *data, struct wl_registry *registry,
;; 			    uint32_t name, const char *interface,
;; 			    uint32_t version);

(defcfun "registry_handle_global" :void
  (data :pointer)
  (registry :pointer)
  (name :uint32)
  (interface :string)
  (version :uint32))

;; static void
;; init_egl(struct display *display, struct window *window)

(defcfun "init_egl" :void
  (display (:pointer (:struct display)))
  (window (:pointer (:struct window))))

;; void create_surface(struct window *window)
(defcfun "create_surface" :void
  (window (:pointer (:struct window))))


;; void init_gl(struct window *window)
(defcfun "init_gl" :void
  (window (:pointer (:struct window))))


;; void registry_handle_global(void *data, struct wl_registry *registry,
;; 			    uint32_t name, const char *interface,
;; 			    uint32_t version);
(defcallback registry-handle-global :void
    ((data :pointer)
     (registry :pointer)
     (name :uint32)
     (interface :string)
     (version :uint32))
  (registry-handle-global data registry name interface version))


;; void registry_handle_global_remove(void *data, struct wl_registry *registry,
;; 				   uint32_t name);

(defcallback registry-handle-global-remove :void
    ((data :pointer)
     (wl-shm :pointer)
     (name :uint32))
  (declare (ignore data wl-shm name)))

;; static const struct wl_registry_listener registry_listener = {
;; 	registry_handle_global,
;; 	registry_handle_global_remove
;; };

(defun create-registry-listener ()
  (let* ((type '(:struct wl-registry-listener))
         (registry-listener (foreign-alloc type :count 1)))
    (setf (foreign-slot-value registry-listener type 'global)
	  (callback registry-handle-global))
    (setf (foreign-slot-value registry-listener type 'global-remove)
	  (callback registry-handle-global-remove))
    registry-listener))

(defparameter *egl-registry-listener*
  (create-registry-listener))


;; static const char *vert_shader_text =
;; 	"uniform mat4 rotation;\n"
;; 	"attribute vec4 pos;\n"
;; 	"attribute vec4 color;\n"
;; 	"varying vec4 v_color;\n"
;; 	"void main() {\n"
;; 	"  gl_Position = rotation * pos;\n"
;; 	"  v_color = color;\n"
;; 	"}\n";

(defparameter *vert-shader-text*
  (format nil (str "uniform mat4 rotation;~%"
		   "attribute vec4 pos;~%"
		   "attribute vec4 color;~%"
		   "varying vec4 v_color;~%"
		   "void main() {~%"
		   "  gl_Position = rotation * pos;~%"
		   "  v_color = color;~%"
		   "}~%")))


;; static const char *frag_shader_text =
;; 	"precision mediump float;\n"
;; 	"varying vec4 v_color;\n"
;; 	"void main() {\n"
;; 	"  gl_FragColor = v_color;\n"
;; 	"}\n";

(defparameter *frag-shader-text*
  (format nil (str "precision mediump float;~%"
		   "varying vec4 v_color;~%"
		   "void main() {~%"
		   "  gl_FragColor = v_color;~%"
		   "}~%")))
;; struct wl_surface *
;; wl_compositor_create_surface_alt(struct wl_compositor *wl_compositor)
(defcfun "wl_compositor_create_surface_alt" :pointer
  (wl-compositor :pointer))

;; void set_display_cursor_surface(struct display* display_ptr, struct wl_surface * wl_surface_ptr)
(defcfun "set_display_cursor_surface" :void
  (dispaly-ptr :pointer)
  (wl-surface-ptr :pointer))


;; int inner_loop (struct window* window_ptr, struct display* display_ptr)
(defcfun "inner_loop" :int
  (window-ptr :pointer)
  (display-ptr :pointer))



;; void
;; redraw(void *data, struct wl_callback *callback, uint32_t time)
(defcfun "redraw" :void
  (window-ptr :pointer)
  (display-ptr :pointer)
  (time :uint32))

(defun run-ulubis-lab (&key (delay 0) (opaque 0) (buffer-size 32)
			 (frame-sync 1) usage (fullscreen 0))
  (with-foreign-objects ((window-sap '(:struct window) 1)
			 (display-sap '(:struct display) 1)
			 (running-sap :int 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-sap
				     :cffi-type '(:struct window)))
	  (display-ptr (make-instance 'display
				      :sap display-sap
				      :cffi-type '(:struct display))))
      (when usage
	(format t (str "Usage: simple-egl [OPTIONS]~%~%"
		       "  -d <us> Buffer swap delay in microseconds~%"
		       "  -f Run in fullscreen mode~%"
		       "  -o Create an opaque surface~%"
		       "  -s Use a 16 bpp EGL config~%"
		       "  -b Don't sync to compositor redraw (eglSwapInterval 0)~%"
		       "  -h This help text~%~%"))
	(return-from run-ulubis-lab))
      (setf (-> window-ptr 'display) display-ptr
	    (-> display-ptr 'window) window-ptr
	    (-> (-> window-ptr 'geometry) 'height) 550
	    (-> (-> window-ptr 'geometry) 'width) 550
	    (-> window-ptr 'window-size) (-> window-ptr 'geometry)
	    (-> window-ptr 'buffer-size) buffer-size
      	    (-> window-ptr 'frame-sync) frame-sync
      	    (-> window-ptr 'delay) delay
	    (-> window-ptr 'fullscreen) fullscreen
	    (-> window-ptr 'opaque) opaque
	    (-> display-ptr 'display) (wl-display-connect (null-pointer))
	    (-> display-ptr 'registry) (wl-display-get-registry
					(-> display-ptr 'display)))
      (wl-registry-add-listener (-> display-ptr 'registry) *egl-registry-listener* (sap display-ptr))
      (init-egl (sap display-ptr) (sap window-ptr))
      (wl-display-roundtrip (-> display-ptr 'display))
      (setf (mem-ref (foreign-symbol-pointer "vert_shader_text") :string)
	    *vert-shader-text*)
      (setf (mem-ref (foreign-symbol-pointer "frag_shader_text") :string)
	    *frag-shader-text*)
      (create-surface (sap window-ptr))
      (init-gl (sap window-ptr))
      (set-display-cursor-surface (sap display-ptr)
				  (wl-compositor-create-surface (-> display-ptr 'compositor)))

      (setf (mem-aref running-sap :int 0) 1)
      (do ()
	  ((not (progn
		  (cond
		    ((-> window-ptr 'wait-for-configure)
		     (wl-display-dispatch (-> display-ptr 'display)))
		    (t
		     (let ((ret (wl-display-dispatch-pending (-> display-ptr 'display))))
		       (redraw (sap window-ptr) (null-pointer) 0)
		       ret))))))))))
