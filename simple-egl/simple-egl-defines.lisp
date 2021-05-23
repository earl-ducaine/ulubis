
(in-package :simple-egl)

(defun str (&rest rest)
  (apply #'concatenate 'string rest))

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
  (window :pointer)
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
  (geometry :pointer)
  (window-size :pointer)
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
  "precision mediump float;\n"
  "varying vec4 v_color;\n"
  "void main() {\n"
  "  gl_FragColor = v_color;\n"
  "}\n")

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

;; static void
;; init_egl(struct display *display, struct window *window)
;; {
;; 	static const struct {
;; 		char *extension, *entrypoint;
;; 	} swap_damage_ext_to_entrypoint[] = {
;; 		{
;; 			.extension = "EGL_EXT_swap_buffers_with_damage",
;; 			.entrypoint = "eglSwapBuffersWithDamageEXT",
;; 		},
;; 		{
;; 			.extension = "EGL_KHR_swap_buffers_with_damage",
;; 			.entrypoint = "eglSwapBuffersWithDamageKHR",
;; 		},
;; 	};
;; 	static const EGLint context_attribs[] = {
;; 		EGL_CONTEXT_CLIENT_VERSION, 2,
;; 		EGL_NONE
;; 	};
;; 	const char *extensions;
;; 	EGLint config_attribs[] = {
;; 		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
;; 		EGL_RED_SIZE, 1,
;; 		EGL_GREEN_SIZE, 1,
;; 		EGL_BLUE_SIZE, 1,
;; 		EGL_ALPHA_SIZE, 1,
;; 		EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
;; 		EGL_NONE
;; 	};
;; 	EGLint major, minor, n, count, i, size;
;; 	EGLConfig *configs;
;; 	EGLBoolean ret;
;; 	if (window->opaque || window->buffer_size == 16)
;; 		config_attribs[9] = 0;
;; 	display->egl.dpy =
;; 		weston_platform_get_egl_display(EGL_PLATFORM_WAYLAND_KHR,
;; 						display->display, NULL);
;; 	assert(display->egl.dpy);
;; 	ret = eglInitialize(display->egl.dpy, &major, &minor);
;; 	assert(ret == EGL_TRUE);
;; 	ret = eglBindAPI(EGL_OPENGL_ES_API);
;; 	assert(ret == EGL_TRUE);
;; 	if (!eglGetConfigs(display->egl.dpy, NULL, 0, &count) || count < 1)
;; 		assert(0);
;; 	configs = calloc(count, sizeof *configs);
;; 	assert(configs);
;; 	ret = eglChooseConfig(display->egl.dpy, config_attribs,
;; 			      configs, count, &n);
;; 	assert(ret && n >= 1);
;; 	for (i = 0; i < n; i++) {
;; 		eglGetConfigAttrib(display->egl.dpy,
;; 				   configs[i], EGL_BUFFER_SIZE, &size);
;; 		if (window->buffer_size == size) {
;; 			display->egl.conf = configs[i];
;; 			break;
;; 		}
;; 	}
;; 	free(configs);
;; 	if (display->egl.conf == NULL) {
;; 		fprintf(stderr, "did not find config with buffer size %d\n",
;; 			window->buffer_size);
;; 		exit(EXIT_FAILURE);
;; 	}
;; 	display->egl.ctx = eglCreateContext(display->egl.dpy,
;; 					    display->egl.conf,
;; 					    EGL_NO_CONTEXT, context_attribs);
;; 	assert(display->egl.ctx);
;; 	display->swap_buffers_with_damage = NULL;
;; 	extensions = eglQueryString(display->egl.dpy, EGL_EXTENSIONS);
;; 	if (extensions &&
;; 	    weston_check_egl_extension(extensions, "EGL_EXT_buffer_age")) {
;; 		for (i = 0; i < (int) ARRAY_LENGTH(swap_damage_ext_to_entrypoint); i++) {
;; 			if (weston_check_egl_extension(extensions,
;; 						       swap_damage_ext_to_entrypoint[i].extension)) {
;; 				/* The EXTPROC is identical to the KHR one */
;; 				display->swap_buffers_with_damage =
;; 					(PFNEGLSWAPBUFFERSWITHDAMAGEEXTPROC)
;; 					eglGetProcAddress(swap_damage_ext_to_entrypoint[i].entrypoint);
;; 				break;
;; 			}
;; 		}
;; 	}
;; 	if (display->swap_buffers_with_damage)
;; 		printf("has EGL_EXT_buffer_age and %s\n", swap_damage_ext_to_entrypoint[i].extension);
;; }


(defun (init-egl display-ptr window-ptr)
  (let (extensions count i size configs-ptr ret
		   (context-attribs
		     (foreign-alloc
		      eglint
		      :count 3
		      :initial-contents '(egl-context-client-version 2 egl-none)))
		   (config-attribs
		     (foreign-alloc
		      eglint
		      :count 13
		      :initial-contents '(egl-surface-type egl-window-bit
					  egl-red-size 1 egl-green-size 1
					  egl-blue-size 1 egl-alpha-size 1
					  egl-renderable-type egl-opengl-es2-bit
					  egl-none))))
    (with-foreign-objects ((major-ptr eglint)
			   (minor-ptr eglint)
			   (minor-ptr eglint)
			   (n eglint)
			   (configs (:struct eglconfig) :count count))
      (with-foreign-slots ((opaque buffer-size) window-ptr
			   (:struct window))
	(when (or (/= opaque 0) (= buffer-size 16))
	  (setf (mem-aref config-attribs eglint 9) 0))
	(assert (/= (setf (foreign-slot-value
			   (foreign-slot-value display-ptr '(:struct display) 'egl)
			   '(:struct egl) 'dpy)
			  (weston-platform-get-egl-display
			   egl-platform-wayland-khr
			   (foreign-slot-value display-ptr '(:struct display) 'display)
			   (null-pointer)))
		    0))
	(assert (= (eglinitialize (foreign-slot-value
				   (foreign-slot-value display-ptr '(:struct display) 'egl)
				   '(:struct egl) 'dpy) major-ptr minor-ptr)
		   egl-true))
	(assert (= (eglbindapi egl-opengl-es-api) egl-true))
	(assert (and (/= (eglgetconfigs
			  (foreign-slot-value
			   (foreign-slot-value display-ptr '(:struct display) 'egl)
			   '(:struct egl) 'dpy)
			  (null-pointer) 0 count) 0)
		     (>= count 1)))
	(setf configs (foreign-alloc (:struct eglconfig) :count count))
	(assert (and (/= (eglchooseconfig
			  (foreign-slot-value
			   (foreign-slot-value display-ptr
					       '(:struct display) 'egl)
			   '(:struct egl) 'dpy)
			  config-attribs
			  configs
			  count) 0)
		     (>= n 1)))
	(dotimes (i n)
	  (eglgetconfigattrib (foreign-slot-value
			       (foreign-slot-value display-ptr
						   '(:struct display) 'egl)
			       '(:struct egl) 'dpy)
			      (mem-aptr configs eglconfig i)
			      egl-buffer-size size)
	  (when (= (foreign-slot-value window-ptr
				       '(:struct window) 'buffer-size)
		   size)
	    (setf (foreign-slot-value
		   (foreign-slot-value display-ptr
				       '(:struct display) 'egl)
		   '(:struct egl) 'conf)
		  (mem-aptr configs eglconfig i))
	    (return)))
	(when (null-pointer-p (foreign-slot-value
		   (foreign-slot-value display-ptr
				       '(:struct display) 'egl)
		   '(:struct egl) 'conf))
	  (error "did not find config with buffer size ~s~%"
		 (foreign-slot-value window-ptr
				     '(:struct window) 'buffer-size)))

	display->egl.ctx = eglcreatecontext(display->egl.dpy
					    display->egl.conf
					    egl-no-context context-attribs)
	assert(display->egl.ctx)
	display->swap-buffers-with-damage = null
	extensions = eglquerystring(display->egl.dpy egl-extensions)
	if (extensions &&
		       weston-check-egl-extension(extensions "egl-ext-buffer-age")) (
		       for (i = 0 i < (int) array-length(swap-damage-ext-to-entrypoint) i++) (
											      if (weston-check-egl-extension(extensions
															     swap-damage-ext-to-entrypoint[i].extension)) (
																					   /* the extproc is identical to the khr one */
																					   display->swap-buffers-with-damage =
																					   (pfneglswapbufferswithdamageextproc)
																					   eglgetprocaddress(swap-damage-ext-to-entrypoint[i].entrypoint)
																					   break
																					   )
											      )
		       )
	if (display->swap-buffers-with-damage)
	printf("has egl-ext-buffer-age and %s\n" swap-damage-ext-to-entrypoint[i].extension)
	))
