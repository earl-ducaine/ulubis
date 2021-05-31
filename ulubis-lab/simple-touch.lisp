
(defpackage :simple-touch
  (:use :common-lisp :split-sequence :cffi :wayland-client-core
  :wayland-client-protocol :wayland-util))

(in-package :simple-touch)

;; (defclass seat ()
;;   ((touch :accessor touch
;; 	  :initarg :touch)
;;    (seat :accessor seat
;; 	 :initarg :seat)
;;    (wl-touch :accessor wl-touch
;; 	     :initarg :wl-touch)))

;; (defclass touch ()
;;   ((display :accessor display :initarg :display)
;;    (registry  :accessor registry :initarg :registry)
;;    (compositor :accessor compositor :initarg :compositor)
;;    (shell :accessor shell :initarg :shell)
;;    (shm :accessor shm :initarg :shm)
;;    (pointer :accessor pointer :initarg :pointer)
;;    (keyboard :accessor keyboard :initarg :keyboard)
;;    (surface :accessor surface :initarg :surface)
;;    (shell-surface :accessor shell-surface :initarg :shell-surface)
;;    (buffer :accessor buffer :initarg :buffer)
;;    (argb-p :accessor argb-p :initarg :argb-p :initform nil)
;;    (width :accessor width :initarg :width)
;;    (height :accessor height :initarg :height)
;;    (data  :accessor data :initarg :data)))

(defconstant prot-read #x02)
(defconstant prot-write #x04)

(defcfun "strerr" :string
  (errnum :int))

(defun create-shm-buffer(touch)
  (with-foreign-slots ((width height data buffer surface shm) touch
                       (:struct touch))
  (let* ((stride (* width 4))
         (size (* stride height))
         (fd (os-create-anonymous-file size)))
    (when (< fd 0)
      ;; (format *error-output* "creating a buffer file for ~d B failed: ~s~%"
      ;;         size (strerror errno))
      (sb-ext:exit :code 1))
    (let ((map (sb-posix:mmap (null-pointer) size
                              (logior sb-posix:prot-read sb-posix:prot-write)
                    sb-posix:map-shared fd 0)))
      (unless map
        ;; (format *error-output* "mmap failed: ~s\~%" (strerror errno))
        (sb-ext::close fd)
        (sb-ext:exit :code 1))
      (setf (foreign-slot-value touch '(:struct touch) 'data) map)
    (let (( pool (wl-shm-create-pool shm fd size)))
      (setf (foreign-slot-value touch '(:struct touch) 'buffer)
	    (wl-shm-pool-create-buffer
             pool 0 width height stride
             (foreign-bitfield-value 'wl-shm-format :argb8888)))
      (wl-shm-pool-destroy pool)
      (sb-posix::close fd))))))


;; struct wl_shm_listener {
;; 	/**
;; 	 * pixel format description
;; 	 *
;; 	 * Informs the client about a valid pixel format that can be used
;; 	 * for buffers. Known formats include argb8888 and xrgb8888.
;; 	 * @param format buffer pixel format
;; 	 */
;; 	void (*format)(void *data,
;; 		       struct wl_shm *wl_shm,
;; 		       uint32_t format);
;; };

(defcstruct wl-shm-listener
  (format :pointer))


;; static void
;; touch_paint(struct touch *touch, int32_t x, int32_t y, int32_t id)
;; {
;; 	uint32_t *p, c;
;; 	static const uint32_t colors[] = {
;; 		0xffff0000,
;; 		0xffffff00,
;; 		0xff0000ff,
;; 		0xffff00ff,
;; 		0xff00ff00,
;; 		0xff00ffff,
;; 	};
;; 	if (id < (int32_t) ARRAY_LENGTH(colors))
;; 		c = colors[id];
;; 	else
;; 		c = 0xffffffff;
;; 	if (x < 2 || x >= touch->width - 2 ||
;; 	    y < 2 || y >= touch->height - 2)
;; 		return;
;; 	p = (uint32_t *) touch->data + (x - 2) + (y - 2) * touch->width;
;; 	p[2] = c;
;; 	p += touch->width;
;; 	p[1] = c;
;; 	p[2] = c;
;; 	p[3] = c;
;; 	p += touch->width;
;; 	p[0] = c;
;; 	p[1] = c;
;; 	p[2] = c;
;; 	p[3] = c;
;; 	p[4] = c;
;; 	p += touch->width;
;; 	p[1] = c;
;; 	p[2] = c;
;; 	p[3] = c;
;; 	p += touch->width;
;; 	p[2] = c;
;; 	wl_surface_attach(touch->surface, touch->buffer, 0, 0);
;; 	wl_surface_damage(touch->surface, x - 2, y - 2, 5, 5);
;; 	/* todo: We could queue up more damage before committing, if there
;; 	 * are more input events to handle.
;; 	 */
;; 	wl_surface_commit(touch->surface);
;; }

(defconstant +wl-surface-attach+ 1)
(defconstant +wl-surface-damage+ 2)
(defconstant +wl-surface-commit+ 6)

(defconstant +wl-registry-bind+ 0)





;; /**
;;  * @ingroup iface_wl_seat
;;  * seat capability bitmask
;;  *
;;  * This is a bitmask of capabilities this seat has; if a member is
;;  * set, then it is present on the seat.
;;  */
;; enum wl_seat_capability {
;; 	/**
;; 	 * the seat has pointer devices
;; 	 */
;; 	WL_SEAT_CAPABILITY_POINTER = 1,
;; 	/**
;; 	 * the seat has one or more keyboards
;; 	 */
;; 	WL_SEAT_CAPABILITY_KEYBOARD = 2,
;; 	/**
;; 	 * the seat has touch devices
;; 	 */
;; 	WL_SEAT_CAPABILITY_TOUCH = 4,
;; };


(defconstant +wl-seat-capability-pointer+ 1
  "the seat has pointer devices")

(defconstant +wl-seat-capability-keyboard+ 2
  "the seat has one or more keyboards")

(defconstant +wl-seat-capability-touch+ 4
  "the seat has touch devices")

(defconstant +wl-seat-get-pointer+ 0)
(defconstant +wl-seat-get-keyboard+ 1)
(defconstant +wl-seat-get-touch+ 2)
(defconstant +wl-seat-release+ 3)



;; 8 bits precision
(defconstant +wl-fixed-precision-factor+ (expt 2 #b1000))


;; Utility structure used in wayland-client-protocol-core.h: wl_fixed_to_double
;; (and friends)
;;
;; (defcunion fixed-uint32
;;   (double :double)
;;   (integer :int64))

(defun print-bytes-little-endian (endian-test)
  (format T "~<~@{~8,1,0,'0@a ~a ~a ~a~_~}~:>"
          (loop for i from 0 below (foreign-type-size '(:union endian-test))
             collect
               (format nil "~8,'0b "
                       (mem-ref endian-test :unsigned-char i)))))

(defun print-bytes-big-endian (endian-test)
  (let ((bits
         (apply #'concatenate 'string
                (loop for i from (- (foreign-type-size '(:union endian-test)) 1)
                   downto 0
                   collect
                     (format nil "~8,'0b"
                             (mem-ref endian-test :unsigned-char i))))))
    (format t "~a ~a ~a" (subseq bits 0 1) (subseq bits 1 12) (subseq bits 12 63))))

(defun pprint-bytes (bytes)
  (pprint-logical-block (nil nil)
    (dolist (byte bytes)
      (pprint-pop)
      (write byte :right-margin 25 :pretty T)
      (write-char #\Space)
      (pprint-newline :fill))))

;; static inline wl_fixed_t
;; wl_fixed_from_double(double d)
;; {
;; 	union {
;; 		double d;
;; 		int64_t i;
;; 	} u;
;; 	u.d = d + (3LL << (51 - 8));
;; 	return (wl_fixed_t)u.i;
;; }

(defun wl-fixed-from-double (double)
    (multiple-value-bind (integer fraction) (truncate double)
      (+ (* integer +wl-fixed-precision-factor+)
         (round (* fraction +wl-fixed-precision-factor+)))))

;; static inline double
;; wl_fixed_to_double(wl_fixed_t f)
;; {
;; 	union {
;; 		double d;
;; 		int64_t i;
;; 	} u;
;; 	u.i = ((1023LL + 44LL) << 52) + (1LL << 51) + f;
;; 	return u.d - (3LL << 43);
;; }

(defun wl-fixed-to-double (fixed-num)
  (coerce (/ fixed-num +wl-fixed-precision-factor+) 'double-float))

(defctype wl-fixed-t :int32
  "A `wl-fixed-t` is a 24.8 signed fixed-point number with a sign bit, 23 bits
  of integer precision and 8 bits of decimal precision.")


(defun touch-paint (touch-ptr x y id)
  (with-foreign-slots ((width height data buffer surface) touch-ptr
                       (:struct touch))
    (let ((colors #(#xffff0000
		    #xffffff00
		    #xff0000ff
		    #xffff00ff
		    #xff00ff00
		    #xff00ffff))
          p c)
      (if (< id (length colors))
	  (setf c (aref colors id))
	  (setf c #xffffffff))
      (unless (or (< x 2) (>= x (- width 2))
	        (< y 2) (>= y (- height 2)))
      ;; p = (uint32_t *) touch->data + (x - 2) + (y - 2) * touch->width;
      (let ((width-increment (* (foreign-type-size :uint32) width)))
        (setf p (mem-aptr data :uint32 (+ (- x 2) (* (- y 2) width))))
        (setf (mem-aref p :uint32 2) c)
        (incf-pointer p width-increment)
        (setf (mem-aref p :uint32 1) c)
        (setf (mem-aref p :uint32 2) c)
        (setf (mem-aref p :uint32 3) c)
        (incf-pointer p width-increment)
        (setf (mem-aref p :uint32 0) c)
        (setf (mem-aref p :uint32 1) c)
        (setf (mem-aref p :uint32 2) c)
        (setf (mem-aref p :uint32 3) c)
        (setf (mem-aref p :uint32 4) c)
        (incf-pointer p width-increment)
        (setf (mem-aref p :uint32 1) c)
        (setf (mem-aref p :uint32 2) c)
        (setf (mem-aref p :uint32 3) c)
        (incf-pointer p width-increment)
        (setf (mem-aref p :uint32 2) c)
        (wl-surface-attach surface buffer 0 0)
        (wl-surface-damage surface (- x 2) (- y 2) 5 5)
        ;; todo: We could queue up more damage before committing if there are
        ;; more input events to handle.
        (wl-surface-commit surface))))))


;; static void
;; touch_handle_down(void *data, struct wl_touch *wl_touch,
;; 		  uint32_t serial, uint32_t time, struct wl_surface *surface,
;; 		  int32_t id, wl_fixed_t x_w, wl_fixed_t y_w)
;; {
;; 	struct touch *touch = data;
;; 	float x = wl_fixed_to_double(x_w);
;; 	float y = wl_fixed_to_double(y_w);
;; 	touch_paint(touch, x, y, id);
;; }

(defcallback touch-handle-down :void
    ((data :pointer)
     (wl-touch :pointer)
     (serial :uint32)
     (time :uint32)
     (surface :pointer)
     (id :uint32)
     (x-w wl-fixed-t)
     (y-w wl-fixed-t))
  (let* ((x (wl-fixed-to-double x-w))
	 (y (wl-fixed-to-double y-w))
         (wl-touch wl-touch)
         (serial serial)
         (time time)
         (surface surface))
    (declare (ignore wl-touch serial time surface))
    (touch-paint data x y id)))


;; static void
;; touch_handle_up(void *data, struct wl_touch *wl_touch,
;; 		uint32_t serial, uint32_t time, int32_t id)
;; {
;; }

(defcallback touch-handle-up :void
    ((data :pointer)
     (wl-touch :pointer)
     (serial :uint32)
     (time :uint32)
     (id :uint32))
  (let ((data data)
        (wl-touch wl-touch)
        (serial serial)
        (time time)
        (id id))
    (declare (ignore data wl-touch serial time id))))

;; static void
;; touch_handle_motion(void *data, struct wl_touch *wl_touch,
;; 		    uint32_t time, int32_t id, wl_fixed_t x_w, wl_fixed_t y_w)
;; {
;; 	struct touch *touch = data;
;; 	float x = wl_fixed_to_double(x_w);
;; 	float y = wl_fixed_to_double(y_w);
;; 	touch_paint(touch, x, y, id);
;; }

(defcallback touch-handle-motion :void
    ((data :pointer)
     (wl-touch :pointer)
     (time :uint32)
     (id :int32)
     (x-w wl-fixed-t)
     (y-w wl-fixed-t))
  (let ((x (wl-fixed-to-double x-w))
        (y (wl-fixed-to-double y-w))
        (wl-touch wl-touch)
        (time time))
    (declare (ignore wl-touch time))
    (touch-paint data x y id)))

;; static void
;; touch_handle_frame(void *data, struct wl_touch *wl_touch)
;; {
;; }

(defcallback touch-handle-frame :void
    ((data :pointer)
     (wl-touch :pointer))
  (let ((data data)
        (wl-touch wl-touch))
    (declare (ignore data wl-touch))))


;; static void
;; touch_handle_cancel(void *data, struct wl_touch *wl_touch)
;; {
;; }

(defcallback touch-handle-cancel :void
    ((data :pointer)
     (wl-touch :pointer))
  (let ((data data)
        (wl-touch wl-touch))
    (declare (ignore data wl-touch))))


;; static const struct wl_touch_listener touch_listener = {
;; 	touch_handle_down,
;; 	touch_handle_up,
;; 	touch_handle_motion,
;; 	touch_handle_frame,
;; 	touch_handle_cancel,
;; };


;; struct wl_touch_listener {
;; 	void (*down)(void *data,
;; 		     struct wl_touch *wl_touch,
;; 		     uint32_t serial,
;; 		     uint32_t time,
;; 		     struct wl_surface *surface,
;; 		     int32_t id,
;; 		     wl_fixed_t x,
;; 		     wl_fixed_t y);
;; 	void (*up)(void *data,
;; 		   struct wl_touch *wl_touch,
;; 		   uint32_t serial,
;; 		   uint32_t time,
;; 		   int32_t id);
;; 	void (*motion)(void *data,
;; 		       struct wl_touch *wl_touch,
;; 		       uint32_t time,
;; 		       int32_t id,
;; 		       wl_fixed_t x,
;; 		       wl_fixed_t y);
;; 	void (*frame)(void *data,
;; 		      struct wl_touch *wl_touch);
;; 	void (*cancel)(void *data,
;; 		       struct wl_touch *wl_touch);
;; 	void (*shape)(void *data,
;; 		      struct wl_touch *wl_touch,
;; 		      int32_t id,
;; 		      wl_fixed_t major,
;; 		      wl_fixed_t minor);
;; 	void (*orientation)(void *data,
;; 			    struct wl_touch *wl_touch,
;; 			    int32_t id,
;; 			    wl_fixed_t orientation);
;; };


(defcstruct wl-touch-listener
  (down :pointer)
  (up :pointer)
  (motion :pointer)
  (frame :pointer)
  (cancel :pointer)
  (shape :pointer)
  (orientation :pointer))


(defparameter *touch-listener*
  (let* ((type '(:struct wl-touch-listener))
         (touch-listener (foreign-alloc type :count 1))
         ;;(let ((touch-listener-ptr (mem-aptr touch-listener type 0))
         (slots '((down touch-handle-down)
                  (up touch-handle-up)
                  (motion touch-handle-motion)
                  (frame touch-handle-frame)
                  (cancel touch-handle-cancel)
                  (shape)
                  (orientation))))
    (dolist (slot slots touch-listener)
      (let ((callback (if (cadr slot)
                          `(callback ,(cadr slot))
                          '(null-pointer))))
        (eval `(setf (foreign-slot-value ,touch-listener ',type ',(car slot))
                     ,callback))))))


;; static void
;; seat_handle_capabilities(void *data, struct wl_seat *wl_seat,
;; 			 enum wl_seat_capability caps)
;; {
;; 	struct seat *seat = data;
;; 	struct touch *touch = seat->touch;
;; 	if ((caps & WL_SEAT_CAPABILITY_TOUCH) && !seat->wl_touch) {
;; 		seat->wl_touch = wl_seat_get_touch(wl_seat);
;; 		wl_touch_set_user_data(seat->wl_touch, touch);
;; 		wl_touch_add_listener(seat->wl_touch, &touch_listener, touch);
;; 	} else if (!(caps & WL_SEAT_CAPABILITY_TOUCH) && seat->wl_touch) {
;; 		wl_touch_destroy(seat->wl_touch);
;; 		seat->wl_touch = NULL;
;; 	}
;; }

(defcallback seat-handle-capabilities :void
    ((data :pointer)
     (wl-seat :pointer)
     ;; Note, bug in C signature: enum wl_seat_capability
     (caps :uint32))
  (let ((type '(:struct seat)))
    (cond
      ((and  (logand caps +wl-seat-capability-touch+)
             (null-pointer-p (foreign-slot-value data type 'wl-touch)))
       (setf (foreign-slot-value data type 'wl-touch)
             (wl-seat-get-touch wl-seat))
       (wl-touch-set-user-data (foreign-slot-value data type 'wl-touch)
                               (foreign-slot-value data type 'touch))
       (wl-touch-add-listener (foreign-slot-value data type 'wl-touch)
                              *touch-listener*
                              (foreign-slot-value data type 'touch)))
      ((and (not (logand caps +wl-seat-capability-touch+))
            (not (null-pointer-p (foreign-slot-value data type 'wl-touch))))
       (wl-touch-destroy (foreign-slot-value data type 'wl-touch))
       (setf (foreign-slot-value data type 'wl-touch) (null-pointer))))))


;; struct wl_seat_listener {
;; 	void (*capabilities)(void *data,
;; 			     struct wl_seat *wl_seat,
;; 			     uint32_t capabilities);
;; 	void (*name)(void *data,
;; 		     struct wl_seat *wl_seat,
;; 		     const char *name);
;; };

(defcstruct wl-seat-listener
  (capabilities :pointer)
  (name :pointer))


;; static const struct wl_seat_listener seat_listener = {
;; 	seat_handle_capabilities,
;; };

(defparameter *seat-listener*
  (let* ((type '(:struct wl-seat-listener))
         (seat-listener (foreign-alloc type :count 1)))
    (let ((seat-listener-ptr (mem-aptr seat-listener type 0)))
      (setf (foreign-slot-value seat-listener-ptr type 'capabilities)
            (callback seat-handle-capabilities))
      seat-listener-ptr)))


;; static void
;; add_seat(struct touch *touch, uint32_t name, uint32_t version)
;; {
;; 	struct seat *seat;
;; 	seat = malloc(sizeof *seat);
;; 	assert(seat);
;; 	seat->touch = touch;
;; 	seat->wl_touch = NULL;
;; 	seat->seat = wl_registry_bind(touch->registry, name,
;; 				      &wl_seat_interface, 1);
;; 	wl_seat_add_listener(seat->seat, &seat_listener, seat);
;; }

(defun add-seat (touch name version)
  (declare (ignore version))
  (let* ((type '(:struct seat))
         (seat-ptr (foreign-alloc type :count 1)))
    ;;    (let ((seat-ptr (mem-aptr seat type 0)))
    (setf (foreign-slot-value seat-ptr type 'touch)
          touch)
    (setf (foreign-slot-value seat-ptr type 'wl-touch)
          (null-pointer))
    (setf (foreign-slot-value seat-ptr '(:struct seat) 'seat)
          (wl-registry-bind
           (foreign-slot-value touch '(:struct touch) 'registry)
           name
           *wl-seat-interface*
           1))
    (wl-seat-add-listener
     (foreign-slot-value seat-ptr type 'seat)
     *seat-listener*
     seat-ptr)))


;; (defun handle-ping (void *data struct wl-shell-surface *shell-surface
;; 	  uint32-t serial)
;; 	(wl-shell-surface-pong shell-surface serial))

;; (defun handle-configure(void *data struct wl-shell-surface *shell-surface
;; 		 uint32-t edges int32-t width int32-t height))




(defcfun "wl_event_loop_add_fd" :pointer
  (loop :pointer)
  (fd :int32)
  (mask :uint32)
  (func :pointer) ; type of wl_event_loop_fd_func_t
  (data :pointer))

;; WL_EXPORT void
;; wl_proxy_set_user_data(struct wl_proxy *proxy, void *user_data)

(defcfun "wl_proxy_set_user_data" :void
  (proxy :pointer)
  (user-data :pointer))

;; WL_EXPORT void
;; wl_proxy_destroy(struct wl_proxy *proxy)

(defcfun "wl_proxy_destroy" :void
  (proxy :pointer))


;; static inline struct wl_touch *
;; wl_seat_get_touch(struct wl_seat *wl_seat)
;; {
;; 	struct wl_proxy *id;
;; 	id = wl_proxy_marshal_constructor((struct wl_proxy *) wl_seat,
;; 			 WL_SEAT_GET_TOUCH, &wl_touch_interface, NULL);
;; 	return (struct wl_touch *) id;
;; }

(defun wl-seat-get-touch (wl-seat)
  (wl-proxy-marshal-constructor
   wl-seat +wl-seat-get-touch+ *wl-touch-interface*
   :pointer (null-pointer)))


;; static inline int
;; wl_touch_add_listener(struct wl_touch *wl_touch,
;; 		      const struct wl_touch_listener *listener, void *data)
;; {
;; 	return wl_proxy_add_listener((struct wl_proxy *) wl_touch,
;; 				     (void (**)(void)) listener, data);
;; }

(defun wl-touch-add-listener (wl-touch listener data)
  (wl-proxy-add-listener wl-touch listener data))


;; static inline void
;; wl_touch_set_user_data(struct wl_touch *wl_touch, void *user_data)
;; {
;; 	wl_proxy_set_user_data((struct wl_proxy *) wl_touch, user_data);
;; }

(defun wl-touch-set-user-data (wl-touch user-data)
  (wl-proxy-set-user-data wl-touch user-data))


;; static inline void
;; wl_touch_destroy(struct wl_touch *wl_touch)
;; {
;; 	wl_proxy_destroy((struct wl_proxy *) wl_touch);
;; }

(defun wl-touch-destroy (wl-touch)
  (wl-proxy-destroy wl-touch))


;; static inline void *
;; wl_registry_bind(struct wl_registry *wl_registry, uint32_t name,
;;                         const struct wl_interface *interface, uint32_t version)
;; {
;; 	struct wl_proxy *id;
;; 	id = wl_proxy_marshal_constructor_versioned((struct wl_proxy *) wl_registry,
;; 			                            WL_REGISTRY_BIND,
;;                                                     interface,
;;                                                     version,
;;                                                     name,
;;                                                     interface->name,
;;                                                     version,
;;                                                     NULL);
;; 	return (void *) id;
;; }

(defun wl-registry-bind (wl-registry name interface version)
  (wl-proxy-marshal-constructor-versioned
   wl-registry
   +wl-registry-bind+
   interface
   version
   :uint32 name
   :string (foreign-slot-value interface '(:struct wl-interface) 'name)
   :uint32 version
   :pointer (null-pointer)))


;; struct wl_list {
;; 	/** Previous list element */
;; 	struct wl_list *prev;
;; 	/** Next list element */
;; 	struct wl_list *next;
;; };

;; (defcstruct wl-list
;;   (prev :pointer)
;;   (next :pointer))

;; struct wl_signal {
;; 	struct wl_list listener_list;
;; };

;; (defcstruct wl-signal
;;   (listener_list (:struct wl-list)))


;; struct wl_registry_interface {
;; 	/**
;; 	 * bind an object to the display
;; 	 *
;; 	 * Binds a new, client-created object to the server using the
;; 	 * specified name as the identifier.
;; 	 * @param name unique numeric name of the object
;; 	 * @param interface name of the objects interface
;; 	 * @param version version of the objects interface
;; 	 * @param id bounded object
;; 	 */
;; 	void (*bind)(struct wl_client *client,
;; 		     struct wl_resource *resource,
;; 		     uint32_t name,
;; 		     const char *interface, uint32_t version, uint32_t id);
;; };

;; We'll see if we can avoid defining the following cstructs (we already have a
;; non-c based classes with these name
;; - wl-client
;; - wl-resource

;; (defcstruct wl-registry-interface
;;   (bind :pointer)
;;   (resource :pointer)
;;   (name :uint32)
;;   (interface :pointer)
;;   (version :uint32)
;;   (id :uint32))

;; Note wl-message lives in wayland-util.lisp

;; (defcstruct wl-message
;;   (name :string)
;;   (signature :string)
;;   (types (:pointer (:pointer (:struct wl-interface)))))

;; struct wl_interface {
;; 	/** Interface name */
;; 	const char *name;
;; 	/** Interface version */
;; 	int version;
;; 	/** Number of methods (requests) */
;; 	int method_count;
;; 	/** Method (request) signatures */
;; 	const struct wl_message *methods;
;; 	/** Number of events */
;; 	int event_count;
;; 	/** Event signatures */
;; 	const struct wl_message *events;
;; };

;; (defcstruct wl-interface
;;   (name :string)
;;   (version :int)
;;   (method-count :int)
;;   (methods :pointer)
;;   (event-count :int)
;;   (events :pointer))


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



;; WL_EXPORT const struct wl_interface wl_registry_interface = {
;; 	"wl_registry", 1,
;; 	1, wl_registry_requests,
;; 	2, wl_registry_events,
;; };
;;  */

;; (defcstruct wl-interface
;;   (name :string)
;;   (version :int)
;;   (method-count :int)
;;   (methods (:pointer (:struct wl-message)))
;;   (event-count :int)
;;   (events (:pointer (:struct wl-message))))





;; Global struct objects


(defparameter *wayland-types*
  (let ((initial-contents
         (list (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               *wl-callback-interface*
               *wl-registry-interface*
               *wl-surface-interface*
               *wl-region-interface*
               *wl-buffer-interface*
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               *wl-shm-pool-interface*
               (null-pointer)
               (null-pointer)
               *wl-data-source-interface*
               *wl-surface-interface*
               *wl-surface-interface*
               (null-pointer)
               *wl-data-source-interface*
               (null-pointer)
               *wl-data-offer-interface*
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               *wl-data-offer-interface*
               *wl-data-offer-interface*
               *wl-data-source-interface*
               *wl-data-device-interface*
               *wl-seat-interface*
               *wl-shell-surface-interface*
               *wl-surface-interface*
               *wl-seat-interface*
               (null-pointer)
               *wl-seat-interface*
               (null-pointer)
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               (null-pointer)
               *wl-output-interface*
               *wl-seat-interface*
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               (null-pointer)
               *wl-output-interface*
               *wl-buffer-interface*
               (null-pointer)
               (null-pointer)
               *wl-callback-interface*
               *wl-region-interface*
               *wl-region-interface*
               *wl-output-interface*
               *wl-output-interface*
               *wl-pointer-interface*
               *wl-keyboard-interface*
               *wl-touch-interface*
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               *wl-surface-interface*
               (null-pointer)
               (null-pointer)
               (null-pointer)
               *wl-subsurface-interface*
               *wl-surface-interface*
               *wl-surface-interface*
               *wl-surface-interface*
               *wl-surface-interface*)))
    (foreign-alloc '(:pointer (:struct wl-message))
                   :initial-contents initial-contents)))

;; static const struct wl_message wl_compositor_requests[] = {
;; 	{ "create_surface", "n", wayland_types + 10 },
;; 	{ "create_region", "n", wayland_types + 11 },
;; };

(defparameter *wl-compositor-requests*
  (let* ((type '(:struct wl-message))
         (wl-compositor-requests (foreign-alloc type :count 2)))
    (let ((ptr (mem-aptr wl-compositor-requests type 0)))
        (setf (foreign-slot-value ptr type 'name)
              "create_surface")
        (setf (foreign-slot-value ptr type 'signature)
              "n")
        (setf (foreign-slot-value ptr type 'types)
              (mem-aptr *wayland-types* '(:pointer (:struct wl-interface)) 10))
        ;; (eval `(setf (foreign-slot-value ,ptr ',type 'types)
        ;;              ,(aref *wayland-types* 10)))
        )
    (let ((ptr (mem-aptr wl-compositor-requests type 1)))
        (setf (foreign-slot-value ptr type 'name)
              "create_region")
        (setf (foreign-slot-value ptr type 'signature)
              "n")
        (setf (foreign-slot-value ptr type 'types)
              (mem-aptr *wayland-types* '(:pointer (:struct wl-interface)) 11))
        ;; (eval `(setf (foreign-slot-value ,ptr ',type 'types)
        ;;              ,(aref *wayland-types* 11)))
        )
    wl-compositor-requests))



;; WL_EXPORT const struct wl_interface wl_compositor_interface = {
;; 	"wl_compositor", 4,
;; 	2, wl_compositor_requests,
;; 	0, NULL,
;; };

(defparameter *wl-compositor-interface*
  (let* ((type '(:struct wl-interface))
         (interface-ptr (foreign-alloc type :count 1)))
    ;; (let* ((ptr (mem-aptr wl-compositor-interface type 0)))
    (setf (foreign-slot-value interface-ptr type 'name) "wl_compositor")
    (setf (foreign-slot-value interface-ptr type 'version) 4)
    (setf (foreign-slot-value interface-ptr type 'method-count) 2)
    (setf (foreign-slot-value interface-ptr type 'methods) *wl-compositor-requests*)
    (setf (foreign-slot-value interface-ptr type 'event-count) 0)
    (setf (foreign-slot-value interface-ptr type 'events) (null-pointer))
    interface-ptr))


;; static void
;; handle_global(void *data, struct wl_registry *registry,
;; 	      uint32_t name, const char *interface, uint32_t version)
;; {
;; 	struct touch *touch = data;
;; 	if (strcmp(interface, "wl_compositor") == 0) {
;; 		touch->compositor =
;; 			wl_registry_bind(registry, name,
;; 					 &wl_compositor_interface, 1);
;; 	} else if (strcmp(interface, "wl_shell") == 0) {
;; 		touch->shell =
;; 			wl_registry_bind(registry, name,
;; 					 &wl_shell_interface, 1);
;; 	} else if (strcmp(interface, "wl_shm") == 0) {
;; 		touch->shm = wl_registry_bind(registry, name,
;; 					      &wl_shm_interface, 1);
;; 		wl_shm_add_listener(touch->shm, &shm_listener, touch);
;; 	} else if (strcmp(interface, "wl_seat") == 0) {
;; 		add_seat(touch, name, version);
;; 	}
;; }

(defcallback handle-global :void
    ((data :pointer)
     (registry :pointer)
     (name :uint32)
     (interface :string)
     (version :uint32))
  (let ((type '(:struct touch)))
    (cond
      ((string= interface "wl_compositor")
       (setf (foreign-slot-value data type 'compositor)
             (wl-registry-bind registry name *wl-compositor-interface* 1)))
      ((string= interface "wl_shell")
       (setf (foreign-slot-value data type 'shell)
             (wl-registry-bind registry name *wl-shell-interface* 1)))
      ((string= interface "wl_shm")
       (setf (foreign-slot-value data type 'shm)
             (wl-registry-bind registry name *wl-shm-interface* 1))
       (wl-shm-add-listener (foreign-slot-value data type 'shm)
                            *shm-listener* data))
      ((string= interface "wl_seat")
       (add-seat data name version)))))

;; static void
;; handle_global_remove(void *data, struct wl_registry *registry, uint32_t name)
;; {
;; }

(defcallback handle-global-remove :void
    ((data :pointer '(struct touch))
     (registry :pointer)
     (name :uint32))
  (let ((data data)
        (registry registry)
        (name name))
    (declare (ignore data registry name))))


;; static const struct wl_registry_listener registry_listener = {
;; 	handle_global,
;; 	handle_global_remove
;; };

(defparameter *registry-listener*
  (let* ((type '(:struct wl-registry-listener))
         (registry-listener (foreign-alloc type :count 1)))
      (setf (foreign-slot-value registry-listener type 'global)
            (callback handle-global))
      (setf (foreign-slot-value registry-listener type 'global-remove)
            (callback handle-global-remove))
      registry-listener))


;; static const struct wl_message wl_registry_requests[] = {
;; 	{ "bind", "usun", wayland_types + 0 },
;; };

(defparameter *wl-registry-requests*
  (let ((wl-registry-requests (foreign-alloc '(:struct wl-message) :count 1))
        ;; (types (foreign-alloc '(:pointer (:pointer (:struct wl-interface)))
        ;;                      :initial-element (null-pointer)))
        )
    (let* ((type '(:struct wl-message))
           (ptr (mem-aptr wl-registry-requests type 0)))
      (setf (foreign-slot-value ptr type 'name)
            "bind")
      (setf (foreign-slot-value ptr type 'signature)
            "usun")
      (setf (foreign-slot-value ptr type 'types)
            (mem-aptr *wayland-types* '(:pointer (:struct wl-interface)) 0))
      wl-registry-requests)))


;; WL_EXPORT const struct wl_interface wl_shm_interface = {
;; 	"wl_shm", 1,
;; 	1, wl_shm_requests,
;; 	1, wl_shm_events,
;; };

(defvar *wl-shm-interface*
  (let ((wl-registry-interface (foreign-alloc '(:struct wl-interface) :count 1)))
    (let* ((type '(:struct wl-interface))
          (ptr (mem-aptr wl-registry-interface type 0)))
        (setf (foreign-slot-value ptr type 'name)
              "wl_shm")
        (setf (foreign-slot-value ptr type 'version)
              1)
        (setf (foreign-slot-value ptr type 'method-count)
              1)
        (setf (foreign-slot-value ptr type 'methods)
              *wl-shm-requests*)
        (setf (foreign-slot-value ptr type 'event-count)
              1)
        (setf (foreign-slot-value ptr type 'events)
              *wl-shm-events*))))

;; static const struct wl_message wl_registry_events[] = {
;; 	{ "global", "usu", wayland_types + 0 },
;; 	{ "global_remove", "u", wayland_types + 0 },
;; };

(defparameter *wl-registry-events*
  (let ((wl-registry-events (foreign-alloc '(:struct wl-message) :count 2))
        ;; (types (foreign-alloc '(:pointer (:pointer (:struct wl-interface)))
        ;;                      :initial-element (null-pointer)))
        )
    (let ((type '(:struct wl-message)))
      (let ((ptr (mem-aptr wl-registry-events type 0)))
        (setf (foreign-slot-value ptr type 'name)
              "global")
        (setf (foreign-slot-value ptr type 'signature)
              "usu")
        (setf (foreign-slot-value ptr type 'types)
              (mem-aptr *wayland-types* '(:pointer (:struct wl-interface)) 0))
        ;; (setf (foreign-slot-value ptr type 'types)
        ;;       types)
        )
      (let ((ptr (mem-aptr wl-registry-events type 1)))
        (setf (foreign-slot-value ptr type 'name)
              "global_remove")
        (setf (foreign-slot-value ptr type 'signature)
              "u")
        (setf (foreign-slot-value ptr type 'types)
              (mem-aptr *wayland-types* '(:pointer (:struct wl-interface)) 0))
        ;; (setf (foreign-slot-value ptr type 'types)
        ;;       types)
        ))
    wl-registry-events))


;; (defparameter *wl-registry-events*
;;   (let ((wl-registry-events (foreign-alloc '(:struct wl-message) :count 2))
;;         (type (foreign-alloc :pointer :initial-element (null-pointer))))
;;     (with-foreign-slots ((name signature types)
;;                          (mem-ref wl-registry-events '(:struct wl-message) 0)
;;                          (:struct wl-message))
;;       ;; (setf name "global"
;;       ;;       signature "usu"
;;       ;;       types type)
;;       )
;;     (with-foreign-slots ((name signature types)
;;                          (mem-ref wl-registry-events '(:struct wl-message) 1)
;;                          (:struct wl-message))
;;       ;; (setf name "global_remove"
;;       ;;       signature "u"
;;       ;;       types type)
;;       )
;;     wl-registry-events))



(defparameter *wl-registry-interface*
  (let ((wl-registry-interface (foreign-alloc '(:struct wl-interface) :count 1)))
    (let* ((type '(:struct wl-interface))
	   (ptr (mem-aptr wl-registry-interface type 0)))
      (setf (foreign-slot-value ptr type 'name)
	    "wl_registry")
      (setf (foreign-slot-value ptr type 'version)
	    1)
      (setf (foreign-slot-value ptr type 'method-count)
	    1)
      (setf (foreign-slot-value ptr type 'methods)
	    *wl-registry-requests*)
      (setf (foreign-slot-value ptr type 'event-count)
	    2)
      (setf (foreign-slot-value ptr type 'events)
	    *wl-registry-events*)
      wl-registry-interface)))


;; struct seat {
;; 	struct touch *touch;
;; 	struct wl_seat *seat;
;; 	struct wl_touch *wl_touch;
;; };

(defcstruct seat
  (touch :pointer)
  (seat :pointer)
  (wl-touch :pointer))

;; struct touch {
;; 	struct wl_display *display;
;; 	struct wl_registry *registry;
;; 	struct wl_compositor *compositor;
;; 	struct wl_shell *shell;
;; 	struct wl_shm *shm;
;; 	struct wl_pointer *pointer;
;; 	struct wl_keyboard *keyboard;
;; 	struct wl_surface *surface;
;; 	struct wl_shell_surface *shell_surface;
;; 	struct wl_buffer *buffer;
;; 	int has_argb;
;; 	int width, height;
;; 	void *data;
;; };

(defcstruct touch
  (display :pointer)
  (registry :pointer)
  (compositor :pointer)
  (shell :pointer)
  (shm :pointer)
  (pointer :pointer)
  (keyboard :pointer)
  (surface :pointer)
  (shell-surface :pointer)
  (buffer :pointer)
  (has-argb :int)
  (width :int)
  (height :int)
  (data :pointer))


;; struct wl_shell_surface_listener {
;; 	void (*ping)(void *data,
;; 		     struct wl_shell_surface *wl_shell_surface,
;; 		     uint32_t serial);
;; 	void (*configure)(void *data,
;; 			  struct wl_shell_surface *wl_shell_surface,
;; 			  uint32_t edges,
;; 			  int32_t width,
;; 			  int32_t height);
;; 	void (*popup_done)(void *data,
;; 			   struct wl_shell_surface *wl_shell_surface);
;; };

(defcstruct wl-shell-surface-listener
  (ping :pointer)
  (configure :pointer)
  (popup-done :pointer))


;; static void
;; shm_format(void *data, struct wl_shm *wl_shm, uint32_t format)
;; {
;; 	struct touch *touch = data;
;; 	if (format == WL_SHM_FORMAT_ARGB8888)
;; 		touch->has_argb = 1;
;; }

(defcallback shm-format :void
    ((data :pointer)
     (wl-shm :pointer)
     (format :uint32))
  (declare (ignore wl-shm))
  (when (= format  (foreign-bitfield-value 'wl-shm-format :argb8888))
    (setf (foreign-slot-value data '(:struct touch) 'has-argb)
          1)))


;; static void
;; handle_ping(void *data, struct wl_shell_surface *shell_surface,
;; 	    uint32_t serial)
;; {
;; 	wl_shell_surface_pong(shell_surface, serial);
;; }

(defcallback handle-ping :void
    ((data :pointer)
     (shell-surface :pointer)
     (serial :uint32))
  (let ((data data))
    (declare (ignore data))
    (wl-shell-surface-pong shell-surface serial)))


;; static void
;; handle_configure(void *data, struct wl_shell_surface *shell_surface,
;; 		 uint32_t edges, int32_t width, int32_t height)
;; {
;; }

(defcallback handle-configure :void
	((data :pointer)
	 (shell-surface :pointer)
	 (edges :uint32)
	 (width :uint32)
	 (height :uint32))
      (let ((data data)
	    (shell-surface shell-surface)
	    (edges edges)
	    (width width)
	    (height height))
	(declare (ignore data shell-surface edges width height))))


;; static void
;; handle_popup_done(void *data, struct wl_shell_surface *shell_surface)
;; {
;; }

(defcallback handle-popup-done :void
	((data :pointer)
	 (shell-surface :pointer))
      (let ((data data)
	    (shell-surface shell-surface))
	(declare (ignore data shell-surface))))


;; static inline struct wl_registry* wl_display_get_registry(struct wl_display
;;                                                                  *wl_display)
;; {
;; 	struct wl_proxy *registry;
;; 	registry = wl_proxy_marshal_constructor((struct wl_proxy *) wl_display,
;; 			 WL_DISPLAY_GET_REGISTRY, &wl_registry_interface, NULL);
;; 	return (struct wl_registry *) registry;
;; }

;; WL_EXPORT void
;; wl_proxy_marshal(struct wl_proxy *proxy, uint32_t opcode, ...)

(defcfun "wl_proxy_marshal" :void
  (proxy :pointer)
  (opcode :uint32)
  &rest)


;; WL_EXPORT struct wl_proxy *
;; wl_proxy_marshal_constructor(struct wl_proxy *proxy, uint32_t opcode,
;; 			     const struct wl_interface *interface, ...)

(defcfun "wl_proxy_marshal_constructor" :pointer
  (proxy :pointer)
  (opcode :uint32)
  (interface :pointer)
  &rest)

;; WL_EXPORT int
;; wl_display_dispatch_pending(struct wl_display *display)
(defcfun "wl_display_dispatch_pending" :int
  (display :pointer))

;; WL_EXPORT int
;; wl_display_dispatch(struct wl_display *display)
(defcfun "wl_display_dispatch" :int
  (display wl-display-pointer))

;; WL_EXPORT int
;; wl_display_roundtrip(struct wl_display *display)
(defcfun "wl_display_roundtrip" :int
  (display :pointer))


;; WL_EXPORT int
;; wl_display_roundtrip_queue(struct wl_display *display, struct wl_event_queue *queue)
(defcfun "wl_display_roundtrip_queue" :int
  (display :pointer)
  (queue :pointer))

;; WL_EXPORT int
;; wl_proxy_add_listener(struct wl_proxy *proxy,
;; 		      void (**implementation)(void), void *data)
(defcfun "wl_proxy_add_listener" :int
  (proxy :pointer)
  (implementation :pointer)
  (data :pointer))

;; WL_EXPORT struct wl_proxy *
;; wl_proxy_marshal_constructor_versioned(struct wl_proxy *proxy, uint32_t opcode,
;; 				       const struct wl_interface *interface,
;; 				       uint32_t version, ...)
(defcfun "wl_proxy_marshal_constructor_versioned" :pointer
  (proxy :pointer)
  (opcode :uint32)
  (interface :pointer)
  (version :uint32)
  &rest)


;; int
;; os_create_anonymous_file(off_t size);

(defcfun "os_create_anonymous_file" :int
  (proxy :int64))


;; static inline struct wl_shm_pool *
;; wl_shm_create_pool(struct wl_shm *wl_shm, int32_t fd, int32_t size)
;; {
;; 	struct wl_proxy *id;
;; 	id = wl_proxy_marshal_constructor((struct wl_proxy *) wl_shm,
;; 			 WL_SHM_CREATE_POOL, &wl_shm_pool_interface, NULL, fd, size);
;; 	return (struct wl_shm_pool *) id;
;; }

(defcfun "wl_shm_create_pool" :pointer
  (wl-shm :pointer)
  (fd :int32)
  (size :int32))


;; static inline void
;; wl_shell_surface_set_title(struct wl_shell_surface *wl_shell_surface, const char *title)
;; {
;; 	wl_proxy_marshal((struct wl_proxy *) wl_shell_surface,
;; 			 WL_SHELL_SURFACE_SET_TITLE, title);
;; }

(defun wl-shell-surface-set-title (wl-shell-surface title)
  (wl-proxy-marshal wl-shell-surface +wl-shell-surface-set-title+
		    :string title))


;; static inline void
;; wl_surface_set_user_data(struct wl_surface *wl_surface, void *user_data)
;; {
;; 	wl_proxy_set_user_data((struct wl_proxy *) wl_surface, user_data);
;; }

(defun wl-surface-set-user-data (wl-surface user-data)
  (wl-proxy-set-user-data wl-surface user-data))


;; static const struct wl_shell_surface_listener shell_surface_listener = {
;; 	handle_ping,
;; 	handle_configure,
;; 	handle_popup_done
;; };

(defparameter *shell-surface-listener*
  (let ((shell-surface-listener
	  (foreign-alloc '(:struct wl-shell-surface-listener) :count 1)))
    (setf (foreign-slot-value shell-surface-listener
			      '(:struct wl-shell-surface-listener) 'ping)
	  (callback handle-ping))
    (setf (foreign-slot-value shell-surface-listener
			      '(:struct wl-shell-surface-listener) 'configure)
	  (callback handle-configure))
    (setf (foreign-slot-value shell-surface-listener
			      '(:struct wl-shell-surface-listener) 'popup-done)
	  (callback handle-popup-done))
    shell-surface-listener))


;; static inline void
;; wl_shell_surface_set_toplevel(struct wl_shell_surface *wl_shell_surface)
;; {
;; 	wl_proxy_marshal((struct wl_proxy *) wl_shell_surface,
;; 			 WL_SHELL_SURFACE_SET_TOPLEVEL);
;; }

(defun wl-shell-surface-set-toplevel (wl-shell-surface)
  (wl-proxy-marshal wl-shell-surface +wl-shell-surface-set-toplevel+))


;; static inline int
;; wl_shell_surface_add_listener(struct wl_shell_surface *wl_shell_surface,
;; 				     const struct wl_shell_surface_listener *listener,
;; 				     void *data)
;; {
;; 	return wl_proxy_add_listener((struct wl_proxy *) wl_shell_surface,
;; 				     (void (**)(void)) listener, data);
;; }

(defun wl-shell-surface-add-listener (wl-shell-surface listener data)
  (wl-proxy-add-listener wl-shell-surface listener data))


;; static inline void
;; wl_shell_surface_pong(struct wl_shell_surface *wl_shell_surface, uint32_t serial)
;; {
;; 	wl_proxy_marshal((struct wl_proxy *) wl_shell_surface,
;; 			 WL_SHELL_SURFACE_PONG, serial);
;; }

(defun wl-shell-surface-pong (wl-shell-surface serial)
  (wl-proxy-marshal wl-shell-surface +wl-shell-surface-pong+ :uint32 serial))


;; static inline void
;; wl_shm_pool_destroy(struct wl_shm_pool *wl_shm_pool)
;; {
;; 	wl_proxy_marshal((struct wl_proxy *) wl_shm_pool,
;; 			 WL_SHM_POOL_DESTROY);
;; 	wl_proxy_destroy((struct wl_proxy *) wl_shm_pool);
;; }
(defun wl-shm-pool-destroy (wl-shm-pool)
  (wl-proxy-marshal wl-shm-pool +wl-shm-pool-destroy+)
  (wl-proxy-destroy wl-shm-pool))


;; static inline struct wl_shm_pool *
;; wl_shm_create_pool(struct wl_shm *wl_shm, int32_t fd, int32_t size)
;; {
;; 	struct wl_proxy *id;
;; 	id = wl_proxy_marshal_constructor((struct wl_proxy *) wl_shm,
;; 			 WL_SHM_CREATE_POOL, &wl_shm_pool_interface, NULL, fd, size);
;; 	return (struct wl_shm_pool *) id;
;; }
(defun wl-shm-create-pool (wl-shm fd size)
  (wl-proxy-marshal-constructor wl-shm +wl-shm-create-pool+
                                *wl-shm-pool-interface*
                                :pointer (null-pointer)
                                :int32 fd
                                :int32 size))


;; static inline struct wl_buffer *
;; wl_shm_pool_create_buffer(struct wl_shm_pool *wl_shm_pool, int32_t offset,
;;                                  int32_t width, int32_t height, int32_t stride,
;;                                  uint32_t format)
;; {
;; 	struct wl_proxy *id;
;; 	id = wl_proxy_marshal_constructor((struct wl_proxy *) wl_shm_pool,
;; 			                  WL_SHM_POOL_CREATE_BUFFER,
;;                                           &wl_buffer_interface,
;;                                           NULL,
;;                                           offset, width, height, stride,
;;                                           format);
;; 	return (struct wl_buffer *) id;
;; }

(defun wl-shm-pool-create-buffer (wl-shm-pool offset width height stride format)
  (wl-proxy-marshal-constructor wl-shm-pool +wl-shm-pool-create-buffer+
                                *wl-buffer-interface*
                                :pointer (null-pointer)
                                :int32 offset
                                :int32 width
                                :int32 height
                                :int32 stride
                                :uint32 format))


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


;; struct wl_shm_listener shm_listener = {
;; 	shm_format
;; };

(defparameter *shm-listener*
  (let* ((type '(:struct wl-shm-listener))
         (wl-compositor-requests (foreign-alloc type :count 1)))
    ;;    (let ((ptr (mem-aptr wl-compositor-requests type 0)))
    (setf (foreign-slot-value wl-compositor-requests type 'format)
          (callback shm-format))
    wl-compositor-requests))

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

;; (wl-proxy-marshal wl-shell +wl-shell-get-shell-surface+
;;                     :pointer *wl-shell-surface-interface*
;;                     :pointer (null-pointer)
;;                     :pointer surface))


(defun set-touch-slot (touch-ptr &rest rest)
  (when (/= (mod (length rest) 2) 0)
    (error "Wrong number of arguments, rest(~d) should be even~%"
           (length rest)))
  (do* ((pairs rest (cddr pairs)))
       ((null pairs))
    (setf (foreign-slot-value touch-ptr '(:struct touch) (car pairs))
          (cadr pairs))))

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


(defun touch-create (width height touch-ptr)
  (set-touch-slot touch-ptr 'display (wl-display-connect "wayland-1"))
  (set-touch-slot touch-ptr 'has-argb 0)
  (set-touch-slot touch-ptr 'registry
                  (wl-display-get-registry
                   (foreign-slot-value touch-ptr '(:struct touch) 'display)))
  (wl-registry-add-listener (foreign-slot-value touch-ptr '(:struct touch)
                                                'registry)
                            *registry-listener*
                            touch-ptr)
  (wl-display-dispatch
   (foreign-slot-value touch-ptr '(:struct touch) 'display))
  (wl-display-roundtrip
   (foreign-slot-value touch-ptr '(:struct touch) 'display))
  (let ((compositor (foreign-slot-value touch-ptr '(:struct touch)
                                        'compositor)))
    (set-touch-slot touch-ptr
                    'width width
                    'height height
                    'surface (wl-compositor-create-surface
                              compositor)))
  (set-touch-slot touch-ptr 'shell-surface
                  (wl-shell-get-shell-surface
                   (foreign-slot-value touch-ptr '(:struct touch) 'shell)
                   (foreign-slot-value touch-ptr '(:struct touch) 'surface)))
  (create-shm-buffer touch-ptr)
  (unless (eql (foreign-slot-value touch-ptr '(:struct touch) 'shell-surface)
	       (null-pointer))
    (wl-shell-surface-add-listener
     (foreign-slot-value touch-ptr '(:struct touch) 'shell-surface)
     *shell-surface-listener* touch-ptr)
    (wl-shell-surface-set-toplevel
     (foreign-slot-value touch-ptr '(:struct touch) 'shell-surface))
    (wl-surface-set-user-data
     (foreign-slot-value touch-ptr '(:struct touch) 'surface) touch-ptr)
    (wl-shell-surface-set-title
     (foreign-slot-value touch-ptr '(:struct touch) 'shell-surface) "simple-touch")
    (let ((data (foreign-slot-value touch-ptr '(:struct touch) 'data)))
      (dotimes (i (* width height 4))
	(setf (mem-ref data :uchar i) 64)))
    (wl-surface-attach (foreign-slot-value touch-ptr '(:struct touch) 'surface)
		       (foreign-slot-value touch-ptr '(:struct touch) 'buffer)
		        0 0)
    (wl-surface-damage (foreign-slot-value touch-ptr '(:struct touch) 'surface)
		       0 0 width height)
    (wl-surface-commit (foreign-slot-value touch-ptr '(:struct touch) 'surface))))

(defun touch-main ()
  (setf (uiop:getenv "WAYLAND_DEBUG") "1")
  (with-foreign-object (touch-ptr '(:struct touch))
    (touch-create 600 500 touch-ptr)
    (let ((return-code 0))
      (loop while (/= return-code -1)
	    do
	       (progn
		 (format t "~s~%" return-code)
		 (setf return-code
		       (wl-display-dispatch
			(foreign-slot-value touch-ptr '(:struct touch) 'display))))))))
