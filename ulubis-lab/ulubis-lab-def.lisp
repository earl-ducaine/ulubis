
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


;;; Pure CL

(defconstant +wl-display-sync+ 0)
(defconstant +wl-display-get-registry+ 1)

(defconstant +wl-compositor-create-surface+ 0)

(defconstant +wl-shell-get-shell-surface+ 0)

(defconstant +wl-shell-surface-pong+ 0)
(defconstant +wl-shell-surface-set-toplevel+ 3)
(defconstant +wl-shell-surface-set-title+ 8)

(defconstant +wl-shm-pool-create-buffer+ 0)
(defconstant +wl-shm-pool-destroy+ 1)

(defconstant +wl-shm-create-pool+ 0)


;; egl opaque pointers
(defctype egldisplay :pointer)
(defctype eglsurface :pointer)
(defctype eglcontext :pointer)
(defctype eglconfig :pointer)
(defctype gluint :uint)


(defcstruct egl
  (dpy egldisplay)
  (ctx eglcontext)
  (conf eglconfig))

(defcstruct window)

;; struct display {
;; 	struct wl_display *display;
;; 	struct wl_registry *registry;
;; 	struct wl_compositor *compositor;
;; 	struct xdg_wm_base *wm_base;
;; 	struct wl_seat *seat;
;; 	struct wl_pointer *pointer;
;; 	struct wl_touch *touch;
;; 	struct wl_keyboard *keyboard;
;; 	struct wl_shm *shm;
;; 	struct wl_cursor_theme *cursor_theme;
;; 	struct wl_cursor *default_cursor;
;; 	struct wl_surface *cursor_surface;
;; 	struct {
;; 		EGLDisplay dpy;
;; 		EGLContext ctx;
;; 		EGLConfig conf;
;; 	} egl;
;; 	struct window *window;
;; 	PFNEGLSWAPBUFFERSWITHDAMAGEEXTPROC swap_buffers_with_damage;
;; };

(defcstruct display
  (display :pointer)
  (registry :pointer)
  (compositor :pointer)
  (wm-base :pointer)
  (seat :pointer)
  (seat :pointer)
  (pointer :pointer)
  (touch :pointer)
  (keyboard :pointer)
  (shm :pointer)
  (cursor-theme :pointer)
  (default-cursor :pointer)
  (cursor-surface :pointer)
  (egl (:struct egl))
  (window (:pointer (:struct window)))
  (swap-buffers-with-damage :pointer))


;; struct geometry {
;; 	int width, height;
;; };

(defcstruct geometry
  (width :int)
  (height :int))


;; struct {
;; 	GLuint rotation_uniform;
;; 	GLuint pos;
;; 	GLuint col;
;; } gl;

(defcstruct gl
  (rotation-uniform gluint)
  (pos gluint)
  (col gluint))


;; struct window {
;; 	struct display *display;
;; 	struct geometry geometry, window_size;
;; 	struct {
;; 		GLuint rotation_uniform;
;; 		GLuint pos;
;; 		GLuint col;
;; 	} gl;
;; 	uint32_t benchmark_time, frames;
;; 	struct wl_egl_window *native;
;; 	struct wl_surface *surface;
;; 	struct xdg_surface *xdg_surface;
;; 	struct xdg_toplevel *xdg_toplevel;
;; 	EGLSurface egl_surface;
;; 	struct wl_callback *callback;
;; 	int fullscreen, maximized, opaque, buffer_size, frame_sync, delay;
;; 	bool wait_for_configure;
;; };

(defcstruct window
  (display :pointer)
  (geometry (:struct geometry))
  (window-size (:struct geometry))
  (gl (:struct gl))
  (benchmark-time :uint32)
  (frames :uint32)
  (native :pointer)
  (surface :pointer)
  (xdg-surface :pointer)
  (xdg-toplevel :pointer)
  (egl-surface eglsurface)
  (callback :pointer)
  (fullscreen :int)
  (maximized :int)
  (opaque :int)
  (buffer-size :int)
  (frame-sync :int)
  (delay :int)
  (wait-for-configure :bool))


(defvar *vert-shader-text*
  (str
   "uniform mat4 rotation;\n"
   "attribute vec4 pos;\n"
   "attribute vec4 color;\n"
   "varying vec4 v_color;\n"
   "void main() {\n"
   "  gl_Position = rotation * pos;\n"
   "  v_color = color;\n"
   "}\n"))


(defvar *frag-shader-text*
  (str
   "precision mediump float;\n"
   "varying vec4 v_color;\n"
   "void main() {\n"
   "  gl_FragColor = v_color;\n"
   "}\n"))

(defparameter *running* 1)

;; static const struct {
;; 	char *extension, *entrypoint;
;; } swap-damage-ext-to-entrypoint[] = {
;; 	{
;; 		.extension = "EGL-EXT-swap-buffers-with-damage",
;; 		.entrypoint = "eglSwapBuffersWithDamageEXT",
;; 	},
;; 	{
;; 		.extension = "EGL-KHR-swap-buffers-with-damage",
;; 		.entrypoint = "eglSwapBuffersWithDamageKHR",
;; 	},
;; };


(defcstruct swap-damage-ext-to-entrypoint
  (extension :string)
  (entrypoint :string))

;; struct wl_registry_listener {
;; 	void (*global)(void *data,
;; 		       struct wl_registry *wl_registry,
;; 		       uint32_t name,
;; 		       const char *interface,
;; 		       uint32_t version);
;; 	void (*global_remove)(void *data,
;; 			      struct wl_registry *wl_registry,
;; 			      uint32_t name);
;; };

(defcstruct wl-registry-listener
  (global :pointer)
  (global-remove :pointer))


(defcfun "app_main" :int
  (window-ptr (:pointer (:struct window)))
  (display-ptr (:pointer (:struct display)))
  (registry-listener (:pointer (:struct wl-registry-listener)))
  (vert-shader-text :string)
  (frag-shader-text :string)
  (wl-surface-ptr :pointer)
  (running (:pointer :int)))


;; WL_EXPORT int
;; wl_proxy_add_listener(struct wl_proxy *proxy,
;; 		      void (**implementation)(void), void *data)
(defcfun "wl_proxy_add_listener" :int
  (proxy :pointer)
  (implementation :pointer)
  (data :pointer))


;; WL_EXPORT int
;; wl_display_roundtrip(struct wl_display *display)
(defcfun "wl_display_roundtrip" :int
  (display :pointer))

;; int
;; wl_display_dispatch(struct wl_display *display);
(defcfun "wl_display_dispatch" :int
  (display (:pointer (:struct display))))

;; int
;; wl_display_dispatch_pending(struct wl_display *display);
(defcfun "wl_display_dispatch_pending" :int
  (display (:pointer (:struct display))))

(defclass cffi-pointer-wrapper ()
  ;; sap: system area pointer.
  ((sap :accessor sap :initarg :sap)
   (cffi-type :accessor cffi-type :initarg :cffi-type)))

(defun aggregate-struct-slot-p (foreign-type slot)
  (eql 'cffi::aggregate-struct-slot
       ;; Undocumented function to get the slot object of a
       ;; struct. The object is either a cffi::aggregate-struct-slot
       ;; or cffi::simple-struct-slot.
       (type-of (cffi::get-slot-info foreign-type slot))))

(defun slot-foreign-type (foreign-type slot)
  ;; Undocumented function to get foreign type of slot.
  (cffi::slot-type (cffi::get-slot-info foreign-type slot)))

(defmethod -> ((ptr cffi-pointer-wrapper) slot)
  (with-slots (sap cffi-type) ptr
    (let ((aggregate-struct-slot-p (aggregate-struct-slot-p cffi-type slot))
	  (slot-foreign-type (slot-foreign-type cffi-type slot)))
      (cond
	((and aggregate-struct-slot-p
	      (eql (car slot-foreign-type) :struct))
	 ;; Slot is a C-style nested structure, we depend on the name of
	 ;; the structure being the same as the name of the wrapper
	 ;; class, e.g. (:struct window) and (defclass window (..) ...)
	 ;;
	 ;; TODO -- we shouldn't need to specify both the lisp class and
	 ;; the foreign type.
	 (make-instance (cadr slot-foreign-type)
			:sap (foreign-slot-pointer sap cffi-type slot)
			:cffi-type slot-foreign-type))
	(t
	 (foreign-slot-value sap cffi-type slot))))))

(defmethod (setf ->) (value (ptr cffi-pointer-wrapper) slot)
  (with-slots (sap cffi-type) ptr
    (let ((aggregate-struct-slot-p (aggregate-struct-slot-p cffi-type slot))
	  (slot-foreign-type (slot-foreign-type cffi-type slot))
	  (slot-sap (foreign-slot-pointer sap cffi-type slot)))
      (cond
	((and aggregate-struct-slot-p
	      (eql (car slot-foreign-type) :struct))
	 (unless (typep value 'cffi-pointer-wrapper)
	   (error  (str "the only vadid value for aggregate-struct-slots are "
			"cffi-pointer-wrapper objeects. Instead an object of "
			"type (~s) was used.~%") (type-of value)))
	 ;; While convoluted, mem-aref retrieves the values of a C
	 ;; struct as a p-list and also allows those values to be
	 ;; setf-ed using a p-list, i.e. because slot-sap is a pointer
	 ;; to an agregate structure slot within an existing
	 ;; structure, it must be updated 'by value', not by providing
	 ;; a pointer to a new C struct.
	 (setf (mem-aref slot-sap slot-foreign-type 0)
	       (mem-aref (sap value) slot-foreign-type 0)))
	(t
	 (setf (foreign-slot-value sap cffi-type slot) value))))))


(define-foreign-type window-struct-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser window-struct))

(define-foreign-type display-struct-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser display-struct))

(define-foreign-type geometry-struct-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser geometry-struct))

(defmethod translate-to-foreign ((value cffi-pointer-wrapper) type)
  (sap value))

(defmethod translate-from-foreign (value (type window-struct-type))
  (make-instance 'window
		 :sap value
		 :cffi-type '(:struct window)))

(defmethod translate-from-foreign (value (type display-struct-type))
  (make-instance 'dispaly
		 :sap value
		 :cffi-type '(:struct display)))

(defmethod translate-from-foreign (value (type geometry-struct-type))
  (make-instance 'geometry
		 :sap value
		 :cffi-type '(:struct geometry)))


(defclass window (cffi-pointer-wrapper)
  ()
  (:default-initargs :cffi-type '(struct window)))

(defclass display (cffi-pointer-wrapper)
  ()
  (:default-initargs :cffi-type '(struct display)))

(defclass geometry (cffi-pointer-wrapper)
  ()
  (:default-initargs :cffi-type '(struct geometry)))


;; static inline void
;; wl_surface_commit(struct wl_surface *wl_surface)
;; {
;; 	wl_proxy_marshal((struct wl_proxy *) wl_surface, WL_SURFACE_COMMIT);
;; }

(defun wl-surface-commit (wl-surface)
  (wl-proxy-marshal wl-surface +wl-surface-commit+))


;; static inline void
;; wl_surface_attach(struct wl_surface *wl_surface, struct wl_buffer *buffer,
;;                          int32_t x, int32_t y)
;; {
;; 	wl_proxy_marshal((struct wl_proxy *) wl_surface,
;; 			 WL_SURFACE_ATTACH, buffer, x, y);
;; }

(defun wl-surface-attach (wl-surface buffer x y)
  (wl-proxy-marshal wl-surface +wl-surface-attach+ :pointer buffer :int32 x
                    :int32 y))

;; static inline void
;; wl_surface_damage(struct wl_surface *wl_surface, int32_t x, int32_t y, int32_t
;;                          width, int32_t height)
;; {
;; 	wl_proxy_marshal((struct wl_proxy *) wl_surface,
;; 			 WL_SURFACE_DAMAGE, x, y, width, height);
;; }

(defun wl-surface-damage (wl-surface x y width height)
  (wl-proxy-marshal wl-surface +wl-surface-damage+ :int32 x :int32 y
                    :int32 width :int32 height ))


;; static inline int
;; wl_registry_add_listener(struct wl_registry *wl_registry,
;; 			 const struct wl_registry_listener *listener, void *data)
;; {
;; 	return wl_proxy_add_listener((struct wl_proxy *) wl_registry,
;; 				     (void (**)(void)) listener, data);
;; }

(defun wl-registry-add-listener (wl-registry listener data)
  (wl-proxy-add-listener wl-registry listener data))

;; static inline int
;; wl_shm_add_listener(struct wl_shm *wl_shm,
;; 		    const struct wl_shm_listener *listener, void *data)
;; {
;; 	return wl_proxy_add_listener((struct wl_proxy *) wl_shm,
;; 				     (void (**)(void)) listener, data);
;; }

(defun wl-shm-add-listener (wl-shm listener data)
  (wl-proxy-add-listener wl-shm listener data))


;; static inline int
;; wl_seat_add_listener(struct wl_seat *wl_seat,
;; 		     const struct wl_seat_listener *listener, void *data)
;; {
;; 	return wl_proxy_add_listener((struct wl_proxy *) wl_seat,
;; 				     (void (**)(void)) listener, data);
;; }

(defun wl-seat-add-listener (wl-shm listener data)
  (wl-proxy-add-listener wl-shm listener data))


;; static inline struct wl_registry *
;; wl_display_get_registry(struct wl_display *wl_display)
;; {
;; 	struct wl_proxy *registry;
;; 	registry = wl_proxy_marshal_constructor((struct wl_proxy *) wl_display,
;; 			 WL_DISPLAY_GET_REGISTRY, &wl_registry_interface, NULL);
;; 	return (struct wl_registry *) registry;
;; }

(defun wl-display-get-registry (wl-display)
  (wl-proxy-marshal-constructor
   wl-display +wl-display-get-registry+ *wl-registry-interface*
   :pointer (null-pointer)))

;; static inline struct wl_shell_surface *
;; wl_shell_get_shell_surface(struct wl_shell *wl_shell, struct wl_surface *surface)
;; {
;; 	struct wl_proxy *id;
;; 	id = wl_proxy_marshal_constructor((struct wl_proxy *) wl_shell,
;; 			 WL_SHELL_GET_SHELL_SURFACE,
;;                                           &wl_shell_surface_interface,
;;                                           NULL, surface);
;; 	return (struct wl_shell_surface *) id;
;; }
(defun wl-shell-get-shell-surface (wl-shell surface)
  (wl-proxy-marshal-constructor wl-shell +wl-shell-get-shell-surface+
                                *wl-shell-surface-interface*
                                :pointer (null-pointer)
                                :pointer surface))


;; static inline struct wl_surface *
;; wl_compositor_create_surface(struct wl_compositor *wl_compositor)
;; {
;; 	struct wl_proxy *id;
;; 	id = wl_proxy_marshal_constructor((struct wl_proxy *) wl_compositor,
;; 			 WL_COMPOSITOR_CREATE_SURFACE, &wl_surface_interface, NULL);
;; 	return (struct wl_surface *) id;
;; }
(defun wl-compositor-create-surface (wl-compositor)
  (wl-proxy-marshal-constructor wl-compositor +wl-compositor-create-surface+
                                *wl-surface-interface* :pointer (null-pointer)))
