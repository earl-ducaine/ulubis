
(in-package :ulubis-lab)

(defparameter *lib-pathname*
  (cl-fad:merge-pathnames-as-directory
   (asdf:system-source-directory :cl-egl-lab)
   (cl-fad:pathname-as-directory "opengles3-book/Common/libCommon.so")))

(defun prep-foreign-library-app-main ()
  (eval `(cffi:define-foreign-library lib-common
	   (:unix ,*lib-pathname*)))
  (cffi:use-foreign-library lib-common))

(prep-foreign-library-app-main)

;; The general protocol for all c-spec derived classes is that the
;; objects they create have slots allocated based on the contents of
;; their respective parsed xml entries, while their methods corespond
;; to returned cffi values.

(defclass c-spec ()
  ())

(defclass c-anonymous-entry-spec (c-spec)
  ((id :initarg :id)))

(defclass c-named-entry-spec (c-spec)
  ((name :initarg :name)))

(defclass c-entry-spec (c-anonymous-entry-spec c-named-entry-spec)
  ())

(defclass function-spec (c-entry-spec)
  ((returns :initarg :returns :initform nil)
   (children  :initarg :children :initform nil)))

(defclass typedef-spec (c-entry-spec)
  ((type :initarg type :initform nil)))

(defclass array-type-spec (c-entry-spec)
  ((min :initarg min :initform nil)
   (max :initarg max :initform nil)
   (type :initarg type :initform nil)))

(defclass cv-qualified-type-spec (c-anonymous-entry-spec)
  ((type :initarg type :initform nil)))

(defclass enumeration-spec (c-entry-spec)
  ((children  :initarg :children :initform nil)))

(defclass enum-value-spec (c-named-entry-spec)
  ((init :initarg init :initform nil)))

(defclass field-spec (c-entry-spec)
  ((type :initarg type :initform nil)))

(defclass file-spec (c-entry-spec)
  ())

(defclass function-type-spec (c-entry-spec)
  ())

(defclass fundamental-type-spec (c-entry-spec)
  ())

(defclass namespace-spec (c-entry-spec)
  ())

(defclass pointer-type-spec (c-anonymous-entry-spec)
  ((type :initarg type :initform nil)))

(defclass slotted-type-spec (c-entry-spec)
  ((members :initarg members :initform nil)
   (size :initarg size :initform nil)))

(defclass struct-spec (slotted-type-spec)
  ())

(defclass union-spec (slotted-type-spec)
  ())

(defclass unimplemented-spec (c-entry-spec)
  ())

(defclass variable-spec (c-entry-spec)
  ())

(defclass argument-spec (c-named-entry-spec)
  ((type :initarg type :initform nil)))

(defclass argument-spec (c-anonymous-entry-spec)
  ((name :initarg name :initform nil)
   (type :initarg type :initform nil)))

(defclass ellipsis-spec (c-spec)
  ())

;; Info functions

(defun print-c-spec-classes ()
  (let ((classes '()))
    (labels ((get-sub-classes (class)
	       (dolist (sub-class (closer-mop:class-direct-subclasses class))
		 (unless (member sub-class classes)
		   (push sub-class classes))
		 (get-sub-classes sub-class))))
      (get-sub-classes (find-class 'c-spec))
      (dolist (class-name (sort (mapcar (lambda (class)
					  (format nil "~s" (class-name class)))
					classes)
				#'string-lessp))
	(format t "~a~%" (string-downcase class-name))))))



(defmethod print-object ((object function-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s name:~s returns:~s children:~s"
	    (slot-value object 'id)
	    (slot-value object 'name)
	    (slot-value object 'returns)
	    (slot-value object 'children))))

(defmethod print-object ((object enumeration-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s name:~s children:~s"
	    (slot-value object 'id)
	    (slot-value object 'name)
	    (slot-value object 'children))))

(defmethod print-object ((object typedef-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s name:~s type:~s"
	    (slot-value object 'id)
	    (slot-value object 'name)
	    (slot-value object 'type))))

(defmethod print-object ((object enum-value-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "name:~s init:~s"
	    (slot-value object 'name)
	    (slot-value object 'init))))

(defmethod print-object ((object array-type-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s name:~s type:~s"
	    (slot-value object 'type)
	    (slot-value object 'min)
	    (slot-value object 'max))))

(defmethod print-object ((object cv-qualified-type-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s type:~s"
	    (slot-value object 'id)
	    (slot-value object 'type))))

(defmethod print-object ((object pointer-type-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s type:~s"
	    (slot-value object 'id)
	    (slot-value object 'type))))

(defmethod print-object ((object c-entry-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s name:~s"
	    (slot-value object 'id)
	    (slot-value object 'name))))

(defmethod print-object ((object slotted-type-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s name:~s type:~s"
	    (slot-value object 'id)
	    (slot-value object 'name)
	    (slot-value object 'members)
	    (slot-value object 'size))))

(defmethod print-object ((object field-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "id:~s name:~s type:~s"
	    (slot-value object 'id)
	    (slot-value object 'name)
	    (slot-value object 'type))))

(defmethod print-object ((object argument-spec) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "type:~s name:~s"
	    (slot-value object 'type)
	    (slot-value object 'name))))


(defparameter *parsed-xml* '())

(defun function-name-p (spec-entry function-name)
  (if (and
       (listp spec-entry)
       (nth 4 (car spec-entry))
       (string= (nth 4 (car spec-entry)) function-name))
      spec-entry
      nil))

(defun get-function-spec (function-name)
  (let (function-spec)
    (dolist (i *parsed-xml*)
      (let ((name (and (listp (car i)) (nth 4 (car i)))))
	(when (string= function-name name)
	  (setf function-spec i))
	))
    function-spec))

;; Ids are unique in the gcc-xml file(?), so no need to qualify what
;; type.
(defun get-spec-entry-by-id (id)
  (let (spec-entry)
    (dolist (i *parsed-xml*)
      (let ((item-id (and (listp (car i)) (nth 2 (car i)))))
	(when (string= item-id id)
	  (setf spec-entry i))
	))
    spec-entry))

(defun run-get-function-spec ()
  (get-function-spec "glBindBuffer"))

(defun generate-and-load-function-defs (file-name)
  (with-open-file (xml file-name)
    (setf *parsed-xml* (s-xml:parse-xml-string
			(uiop:slurp-stream-string xml)))))

(defun run-generate-and-load-function-defs ()
  (generate-and-load-function-defs
   (concatenate 'string "/home/rett/dev/common-lisp/lambda-delta/asas/cl-egl-lab/"
		"opengles3-book/Chapter_2/Hello_Triangle/grovel-forms.xml"))
  nil)

;; takes name as keyword and translates it to the appropriate c name
;; using special conversions as necessary.
(defun get-attribute (attribute-name entry)
  (let ((attribute-name (intern (cffi:translate-name-to-foreign attribute-name :keyword)
				:keyword)))
    (eval `(destructuring-bind (type  &key ,(intern (symbol-name attribute-name)) &allow-other-keys)
	       ',entry
	     (declare (ignore type))
	     ,(intern (symbol-name attribute-name))))))

(defun run-get-attribute ()
  (get-attribute :id
		 '(:|Typedef| :|id| "_2" :|name| "__int128_t" :|type| "_1630" :|context| "_1"
		   :|location| "f0:0" :|file| "f0" :|line| "0")))

(defparameter *c-declaration-db* nil)

(defparameter *c-declaration-by-id-table* (make-hash-table :test 'equal))

;; Note, no pefix should be a subsequence of another one.
(defparameter *c-ucase-prefixes*  '("GL" "EGL" "128" "XIM"))

(defmethod c-name ((f c-spec))
  (slot-value f 'name))

(defmethod lisp-name ((f c-spec))
  (tweak-translate-camelcase-name (c-name f)))

(defmethod c-name ((f fundamental-type-spec))
  (slot-value f 'name))

(defmethod lisp-name ((type fundamental-type-spec))
  (let* ((type-string (slot-value type 'name))
	 (type-name (intern (string-upcase (substitute #\- #\space type-string)) :keyword)))
    (or (cadr (assoc type-name *aliases*))
	;; emulated (outside of CFFI) fundamental types
	(car (member type-name cffi:*built-in-foreign-types*))
	(error "Unexpected fundamental type: ~S~%" type-string))))

(defmethod get-arguments ((f function-spec))
  (mapcar (lambda (arg)
	    (argument-type arg))
	  (slot-value f 'children)))

(defparameter *example-function-spec*
  '((:|Function| :|id| "_1507" :|name| "eglWaitSync" :|returns| "_1452"
     :|context| "_1" :|location| "f28:296" :|file| "f28" :|line| "296" :|mangled|
     "_Z11eglWaitSync")
    ((:|Argument| :|name| "dpy" :|type| "_1453" :|location| "f28:296" :|file|
      "f28" :|line| "296"))
    ((:|Argument| :|name| "sync" :|type| "_1494" :|location| "f28:296" :|file|
      "f28" :|line| "296"))
    ((:|Argument| :|name| "flags" :|type| "_1451" :|location| "f28:296" :|file|
      "f28" :|line| "296"))))

(defparameter *example-pointer-type-spec*
  '((:|PointerType| :|id| "_1633" :|type| "_2519" :|size| "64" :|align| "64")))

(defun get-specs-of-type (spec-class-name)
  (reduce (lambda (rest spec)
	    (if (typep spec spec-class-name)
		(cons spec rest)
		rest))
	  *c-declaration-db* :initial-value '()))

(defun list-typedefs ()
  (get-specs-of-type 'typedef-spec))

(defun list-functions ()
  (get-specs-of-type 'function-spec))

(defun list-structs ()
  (get-specs-of-type 'struct-spec))

(defun list-fundamental-types ()
  (get-specs-of-type 'fundamental-type-spec))

;; aliases for CFFI built in types
(defparameter *aliases*
  '((:long-long-unsigned-int :unsigned-long-long)
    (:long-long-int :long-long)
    (:long-int :long)
    (:short-int :short)
    (:signed-char :char)
    (:long-unsigned-int :unsigned-long)
    (:short-unsigned-int :unsigned-short)
    (:__int128 --int128)
    (:unsigned-__int128 --unsigned-int128)
    ;; It's quite wrong, of course, to map long doubles to 128bit
    ;; integers.  But at least they're 16 bytes.
    (:long-double --int128)))

(defun print-fundamental-types ()
  (dolist (type (get-fundamental-types))
    (let ((type-name))
      (unless (member (or (cadr (assoc type-name *aliases*))
			  type-name)
		      cffi:*built-in-foreign-types*)
	(format t "~s~%" type-name)))))

(defmethod enum-values ((enumeration-spec enumeration-spec))
  (mapcar (lambda (enum-value)
	    (list (intern  (slot-value enum-value 'name) :keyword)
		  (parse-integer (slot-value enum-value 'init))))
	  (slot-value enumeration-spec 'children)))

(defmethod slots ((struct struct-spec))
  (process-slots struct))

(defmethod slots ((union union-spec))
  (process-slots union))

(defun process-slots (object)
  (unless (slot-value object 'members)
    (return-from process-slots))
  (mapcar
   (lambda (type-string)
     (let* ((field-spec (gethash type-string *c-declaration-by-id-table*))
	    (name (lisp-name field-spec))
	    (c-type (ensure-c-type (slot-value field-spec 'id))))
       (list name c-type)))
   (split-sequence:split-sequence #\space (slot-value object 'members))))

;; Lookup c-type, recursively definined, to cffi, register any types that
;; haven't been registered yet.  Return the cffi form of the type usable in a defctype.
(defun ensure-c-type (type-string)
  (let ((spec-object (gethash type-string *c-declaration-by-id-table*)))
    (etypecase spec-object
      (typedef-spec
       (unless (cffi-foreign-type-defined-p (lisp-name spec-object))
	 ;; Define it to :pointer to avoid any endless recursion caused
	 ;; by circular, mutual definitions.
	 (eval `(cffi:defctype ,(lisp-name spec-object) :pointer))
	 (let ((type (ensure-c-type (slot-value spec-object 'type))))
	   (format t "~s~%" `(cffi:defctype ,(lisp-name spec-object) ,type))
	   (eval `(cffi:defctype ,(lisp-name spec-object) ,type))))
       (lisp-name spec-object))
      ((or pointer-type-spec array-type-spec)
       ;; If we encounter a pointer define it imediately, in case
       ;; there are circular references.  Once we've finished ensuring
       ;; the types of each slot.  Then redefin e the type as a
       ;; (:pointer <type>).
       (list :pointer (ensure-c-type (slot-value spec-object 'type))))
      (field-spec
       (list :pointer (ensure-c-type (slot-value spec-object 'type))))
      ;; There
      (slotted-type-spec
       ;; Note, C structs can be anonymous, i.e. name == "", so
       ;; generate a unique symbol if this one is.
       (let* ((ctype-def-command (etypecase spec-object
				   (struct-spec 'cffi:defcstruct)
				   (union-spec 'cffi:defcunion)))
	      (slot-value-name (lisp-name spec-object))
	      (name (or slot-value-name (gensym)))
	      (size (parse-integer (or (slot-value spec-object 'size)
				       "")
				   :junk-allowed t)))
	 ;; Avoid infinite recursion if two structs have slots of
	 ;; the other's type.
	 (unless (and slot-value-name
		      (cffi-foreign-type-defined-p slot-value-name))
	   (when slot-value-name
	     (eval `(,ctype-def-command ,slot-value-name)))
	   (let ((slots (slots spec-object)))
	     ;; Note, it's possible to have an anonymous C stucture, and
	     ;; refer to it by id, say, in a typedef.
	     (format t "~a~%" `(,ctype-def-command (,name :size ,(type-of size)) ,@slots))
	     (eval `(,ctype-def-command (,name :size ,size) ,@slots))))
	 `(,(etypecase spec-object
	      (struct-spec :struct)
	      (union-spec :union))
	    ,name)))
      (cv-qualified-type-spec
       ;; There are 3 types of C qualifiers: volitile, const and
       ;; restricted.  CFFI doesn't obey any of of those access
       ;; qualifications, put potentially the runtime system might.
       ;; In an ideal world we would probably note these and enforce
       ;; the specified permision unless explicitly overridded by the
       ;; user.
       (ensure-c-type (slot-value spec-object 'type)))
      (function-type-spec
       :pointer)
      (enumeration-spec
       (let ((enum-values (enum-values spec-object))
	     (lisp-name (lisp-name spec-object)))
	 (eval `(cffi:defcenum ,lisp-name ,@enum-values))
	 lisp-name))
      (fundamental-type-spec
       (lisp-name spec-object)))))

;; Should return list of list where each sublist is the pair of lisp
;; argement mame and its type, e.g.
;;
;; '((dpy :egl-display) (sync :egl-sync) (flags :e-g-lint))

;; depends on internal details of CFFI's implementation and doesn't
;; work for native types, i.e. quick and dirty hack.
(defun cffi-foreign-type-defined-p (type)
  (gethash (cons :default type) cffi::*type-parsers*))

;; Additional hacks to handle style variations, inconsistencies or
;; clerical errors in camelcased names.
(defun tweak-translate-camelcase-name (name)
  ;; If we have a match in our prefixes list, make sure the character
  ;; folowing it is capitalized.
  (unless (> (length name) 0)
    (return-from tweak-translate-camelcase-name))
  (let ((name (substitute #\- #\_ name)))
    (dolist (prefix *c-ucase-prefixes*)
      (when (< (length prefix) (length name))
	(let ((index (search prefix name)))
	  (when (and index (= 0 index))
	    (setf (elt name (length prefix))
		  (char-upcase (elt name (length prefix))))))))
    (cffi:translate-camelcase-name name :special-words *c-ucase-prefixes*)))


(defmethod argument-type ((f argument-spec))
  (let ((type (gethash (slot-value f 'type) *c-declaration-by-id-table*)))
    (etypecase type
      (typedef-spec
       (let* ((argument-name (tweak-translate-camelcase-name (c-name type))
		(slot-value arg 'name)
		(argument-type
		 (intern (symbol-name
			  (tweak-translate-camelcase-name (c-name type)))
			 :keyword)
		 (list )))))))))

(defmethod return-type ((f function-spec))
  (ensure-c-type (slot-value f 'returns)))

(defmethod arguments ((f function-spec))
  (mapcar (lambda (child)
	    (list (or (lisp-name child)
		      (gensym))
		  (ensure-c-type (slot-value child 'type))))
	  (slot-value f 'children)))

(defun generate-defcfun (function-spec)
  (let ((c-name (c-name function-spec))
	(lisp-name (lisp-name function-spec))
	(return-type (return-type function-spec))
	(arguments (arguments function-spec)))
    (eval `(cffi:defcfun (,c-name ,lisp-name) ,return-type ,@arguments))))

(defun process-spec-object-childen (child-entries slots)
  (mapcar (lambda (child-entry)
	    (cond
	      ;; :|Ellipsis| is a special argument type in functional
	      ;; specs.
	      ((eq child-entry :|Ellipsis|)
	       (make-instance 'ellipsis-spec))
	      ;; valid for any child type, e.g. function argument,
	      ;; enumeration value, etc.
	      ((consp child-entry)
	       (update-spec-object child-entry slots))
	      (t
	       (error "Unrecognized child type: ~s" child-entry))))
	  child-entries))

(defun update-spec-object (entry slots)
  (let* ((spec-object (make-instance (car slots)))
	 (slots (cdr slots)))
    (mapc (lambda (slot)
	    (if (consp slot)
		(setf (slot-value spec-object 'children)
		      (process-spec-object-childen (cdr entry) slot))
		(setf (slot-value spec-object (intern (symbol-name slot)))
		      (get-attribute slot (car entry)))))
	  slots)
    (let ((id (get-attribute :id (car entry))))
      (when id
	(setf (gethash id *c-declaration-by-id-table*) spec-object)))
    spec-object))

(defparameter my-array-type
  '(:|ArrayType| :|id| "_1634" :|min| "0" :|max| "0" :|type| "_2779"))


;; Notes about alignment:
;;
;; 1) With no exceptions, sysv abi x86-64 specifies that fundamental
;;    types, e.g. char, short, long long, etc., need to have an
;;    alignment that matches their C sizeof value, e.g. sizeof(long
;;    long) == 8 and its alignment is 8.
;;
;; 2) CFFI has no 128 bit data-types (fundamental or otherwise).  It
;;    also has no interface for specifying an arbitrary alignment.
;;    So, user specified, ad hoc, data types that need alignment
;;    greater than 8 need to be allocated in customized way.
;;
;; 3) One approach is to request a 64 bit item, e.g. long long, and
;;    specify enough padding to make sure the item is 16 byte aligned.
;;
;; 4) To improve the efficiency we could put all 128 bit number into
;;    an array.  Each number, after the padding, being stored
;;    sequentially with no space between the numbers.

;; storage for all C 128 bit C objects requiring 16 byte alignment.
(defparameter *128-bit-item-memory* nil)
(defparameter *128-bit-item-count* 0)
;; Padding at the biginning of *128-bit-item-memory*, in bytes.
(defparameter *128-bit-item-alignment-offset* 0)
;; Index into the array, in bytes.
(defparameter *128-bit-item-memory-index* 0)
(defparameter *default-128-bit-item-memory-size* 1024)

;; Allocate if it hasn't been setup yet. Raise error if there's no
;; more room.
(defun ensure-size-128-bit-items-memory ()
  (unless *128-bit-item-memory*
    (setf *128-bit-item-memory*
	  (cffi:foreign-alloc
	   :long-long
	   :count (+ (* *default-128-bit-item-memory-size* 2) 1)))
    (setf *128-bit-item-alignment-padding*
	  (mod (- 16 (mod (cffi:pointer-address *128-bit-item-memory*) 16)) 16))
    (setf *128-bit-item-memory-index* 0)
    (setf *128-bit-item-count* 0))
  (when (> (+ *128-bit-item-memory-index* *128-bit-item-alignment-padding*)
	   (+ (* 1024 16) 16))
    (error "No room to add additional 128 bit fundamental types to *128-bit-item-memory*")))

;; item-id tell us which item in the array.
(defun get-128-bit-item-byte (item-id index)
  (unless (< item-id *128-bit-item-count*)
    (error "No 128-bit item has been alocated with the id: ~s" item-id))
  (cffi:mem-aref
   *128-bit-item-memory*
   :unsigned-char (+ (* item-id 16) *128-bit-item-alignment-padding* index)))

;; item-id tell us which item in the array.
(defun get-128-bit-item-pointer (item-id index)
  (unless (< item-id *128-bit-item-count*)
    (error "No 128-bit item has been alocated with the id: ~s" item-id))
  (cffi:mem-aref
   *128-bit-item-memory*
   :unsigned-char (+ (* item-id 16) *128-bit-item-alignment-padding* index)))


;; item-id tell us which item in the array.
(defun set-128-bit-item-byte (item-id index value)
  (unless (< item-id *128-bit-item-count*)
    (error "No 128-bit item has been alocated with the id: ~s" item-id))
  (setf (cffi:mem-aref
	 *128-bit-item-memory*
	 :unsigned-char (+ (* item-id 16) *128-bit-item-alignment-padding* index))
	value))

;; item-id tells us which item in the array.
(defun allocate-128-bit-item-byte ()
  (incf *128-bit-item-memory-index* (* 16 8))
  (incf *128-bit-item-count*)
  (1- *128-bit-item-count*))

(defun lisp-integer-to-int128 (int128-item-id lisp-integer)
  (dotimes (i 16)
    (set-128-bit-item-byte int128-item-id i (ldb (byte 8 (* i 8)) lisp-integer))))


;; Our custom foreign int128 data type is an index into the
;; *128-bit-item-memory* table.
(defun int128-to-lisp-integer (item-id)
  (let ((lisp-integer 0))
    (dotimes (i 16 lisp-integer)
      (let ((byte (get-128-bit-item-byte item-id i)))
	(incf lisp-integer (* byte (expt 2 (* 8 i))))))))

(defun int128-to-lisp-integer-alt (int128-struct)
  (flet ((get-item-byte (index)
	   (cffi:mem-aref int128-struct :unsigned-char index)))
    (let ((lisp-integer 0))
      (dotimes (i 16 lisp-integer)
	(let ((byte (get-item-byte i)))
	  (incf lisp-integer (* byte (expt 2 (* 8 i)))))))))

(defun run-test-128int ()
  ;; reset memory
  (setf *128-bit-item-memory* nil)
  (ensure-size-128-bit-items-memory)
  (let ((lisp-integer 12345678901234567890)
	(item-id (allocate-128-bit-item-byte)))
    (set-128-bit-item-byte item-id 0 5)
    (get-128-bit-item-byte item-id 0)
    (lisp-integer-to-int128 0 lisp-integer)
    (int128-to-lisp-integer 0)))

(defparameter *invalid-base-types* '("_2" "_3" "_4" "_6" "_34" "_69" "_105" "_106" "_112" "_125" "_130" "_132" "_136"
				     "_138" "_140" "_144" "_142" "_145" "_147"))

(defun register-c-typedefs ()
  (dolist (typedef (list-typedefs))
    (let ((id (slot-value typedef 'id)))
      (unless (member id *invalid-base-types* :test #'string=)
	(ensure-c-type (slot-value typedef 'id))))))

(defclass --int128-t-type (foreign-type)
  ())

(defmethod --int128-t-size ((type --int128-t-type))
  16)

(defmethod foreign-type-alignment ((type --int128-t-type))
  ;; As defined in the x86-64 SYSV API
  16)

(defmethod aggregatep ((type --int128-t-type))
  nil)

(cffi:defcstruct (--int128-t-struct-alt :size 16)
  (bytes (:pointer :unsigned-char)))

(cffi:defcstruct (--unsigned-int128-t :class --unsigned-int128-t)
  (byte-0 :unsigned-char)
  (byte-1 :unsigned-char)
  (byte-2 :unsigned-char)
  (byte-3 :unsigned-char)
  (byte-4 :unsigned-char)
  (byte-5 :unsigned-char)
  (byte-6 :unsigned-char)
  (byte-7 :unsigned-char)
  (byte-8 :unsigned-char)
  (byte-9 :unsigned-char)
  (byte-10 :unsigned-char)
  (byte-11 :unsigned-char)
  (byte-12 :unsigned-char)
  (byte-13 :unsigned-char)
  (byte-14 :unsigned-char)
  (byte-15 :unsigned-char))

;; These should be 16 bit alignment, not
(cffi:defcstruct (--int128-t :class --int128-t)
  (byte-0 :char)
  (byte-1 :char)
  (byte-2 :char)
  (byte-3 :char)
  (byte-4 :char)
  (byte-5 :char)
  (byte-6 :char)
  (byte-7 :char)
  (byte-8 :char)
  (byte-9 :char)
  (byte-10 :char)
  (byte-11 :char)
  (byte-12 :char)
  (byte-13 :char)
  (byte-14 :char)
  (byte-15 :char))

(cffi:defctype --int128 (:struct --int128-t))
(cffi:defctype --unsigned-int128 (:struct --unsigned-int128-t))

;; TODO -- Test to see if it's in range and handle, ahem, negative numbers.
(defun lisp-integer-to-c-int128 (lisp-integer c-int128)
  (dotimes (i 16)
    (setf (cffi:foreign-slot-value
	   c-int128
	   '(:struct --int128-t)
	   (intern (concatenate 'string "BYTE-" (format nil "~d" i))))
	  (ldb (byte 8 (* i 8)) lisp-integer)))
  c-int128)

(defmethod translate-into-foreign-memory (lisp-object (type-name --unsigned-int128-t) c-unsigned-int128)
  (lisp-integer-to-c-int128 lisp-object c-int128))

(defmethod translate-into-foreign-memory (lisp-object (type-name --int128-t) c-int128)
  (lisp-integer-to-c-int128 lisp-object c-int128))

(defmethod translate-from-foreign (c-unsigned-int128 (type-name --unsigned-int128-t))
  (let ((lisp-integer 0))
    (dotimes (i 16)
      (let ((byte (foreign-slot-value
		   c-unsigned-int128
		   '(:struct --unsigned-int128-t)
		   (intern (concatenate 'string "BYTE-" (format nil "~d" i))))))
	(incf lisp-integer (* byte (expt 2 (* i 8))))))
    lisp-integer))

(defmethod translate-from-foreign (c-int128 (type-name --int128-t))
  (let ((lisp-integer 0))
    (dotimes (i 16)
      (let ((byte (foreign-slot-value
		   c-int128
		   '(:struct --int128-t)
		   (intern (concatenate 'string "BYTE-" (format nil "~d" i))))))
	(incf lisp-integer (* byte (expt 2 (* i 8))))))
    lisp-integer))

(cffi:defctype --int128-t-return
    (:wrapper :pointer :from-c print-return-value))

(defun print-return-value (item)
  (format t "item: ~s~%" (type-of item)))

(defun generate-int128-t-from-foreign-memory (foreign-memory-item-id)
  (let ((int128 (cffi:convert-to-foreign nil '(:struct --int128-t))))
    (dotimes (i 16)
      (setf (cffi:foreign-slot-value
	     int128 '(:struct --int128-t)
	     (intern (concatenate 'string "BYTE-" (format nil "~d" i))))
	    (get-128-bit-item-byte foreign-memory-item-id i)))
    int128))

;; call "mult_128" using the cffi primitive
(defun run-mult-128-alt ()
  (let* ((int1-index 0)
	 (int2-index 1)
	 (int1 (cffi:mem-ref *128-bit-item-memory*
			     '(:struct --int128-t)
			     (+ *128-bit-item-alignment-offset*
				(* int1-index 16))))
	 (int2 (cffi:mem-ref *128-bit-item-memory*
			     '(:struct --int128-t)
			     (+ *128-bit-item-alignment-offset*
				(* int2-index 16)))))
    (cffi::foreign-funcall
     "mult_128"
     (:struct --int128-t) (generate-int128-t-from-foreign-memory 0)
     (:struct --int128-t) (generate-int128-t-from-foreign-memory 1)
     (:struct --int128-t))))

(defparameter *x-client-message-event*
  '((:|Typedef| :|id| "_288" :|name| "XClientMessageEvent" :|type| "_287"
     :|context| "_1" :|location| "f21:910" :|file| "f21" :|line| "910")))

(defun load-database ()
  (when (not *parsed-xml*)
    (generate-and-load-function-defs
     "/home/rett/dev/common-lisp/garnet-desktop-lab/grovel-forms.xml"))
  (setf *c-declaration-db* nil)
  (dolist (entry *parsed-xml*)
    (when (listp (car entry))
      (case (caar entry)
	(:|Typedef|
	  (push (update-spec-object entry '(typedef-spec :id :name :type))
		*c-declaration-db*))
	(:|Function|
	  ;; sublist indicates object has
	  (push (update-spec-object entry '(function-spec :id :name :returns (argument-spec :type :name)))
		*c-declaration-db*))
	(:|ArrayType|
	  (push (update-spec-object entry '(array-type-spec :id :name :type :min :max))
		*c-declaration-db*))
	(:|CvQualifiedType|
	  (push (update-spec-object entry '(cv-qualified-type-spec :id :type))
		*c-declaration-db*))
	(:|Enumeration|
	  (push (update-spec-object entry '(enumeration-spec :id :name (enum-value-spec :name :init)))
		*c-declaration-db*))
	(:|Field|
	  (push (update-spec-object entry '(field-spec :id :name :type))
		*c-declaration-db*))
	(:|File|
	  (push (update-spec-object entry  '(file-spec :id :name))
		*c-declaration-db*))
	(:|FunctionType|
	  (push (update-spec-object entry '(function-type-spec :id :name))
		*c-declaration-db*))
	(:|FundamentalType|
	  (push (update-spec-object entry '(fundamental-type-spec :id :name))
		*c-declaration-db*))
	(:|Namespace|
	  (push (update-spec-object entry '(namespace-spec :id :name))
		*c-declaration-db*))
	(:|PointerType|
	  (push (update-spec-object entry '(pointer-type-spec :id :type))
		*c-declaration-db*))
	(:|Struct|
	  (push (update-spec-object entry '(struct-spec :id :name :members :size))
		*c-declaration-db*))
	(:|Unimplemented|
	  (push (update-spec-object entry '(unimplemented-spec :id :name))
		*c-declaration-db*))
	(:|Union|
	  (push (update-spec-object entry '(union-spec :id :name :members :size))
		*c-declaration-db*))
	(:|Variabel|
	  (push (update-spec-object entry '(variable-spec  :id :name))
		*c-declaration-db*))))))


;; From CFFI library.  This is perhaps a way to emulate longer
;; data-types.  Note particularly: foreign-type-alignment.
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass emulated-llong-type (foreign-type) ())
  (defmethod foreign-type-size ((tp emulated-llong-type)) 8)
  (defmethod foreign-type-alignment ((tp emulated-llong-type))
    ;; better than assuming that the alignment is 8
    (foreign-type-alignment :long))
  (defmethod aggregatep ((tp emulated-llong-type)) nil)

  (define-foreign-type emulated-llong (emulated-llong-type)
    ()
    (:simple-parser :long-long))

  (define-foreign-type emulated-ullong (emulated-llong-type)
    ()
    (:simple-parser :unsigned-long-long))

  (defmethod canonicalize ((tp emulated-llong)) :long-long)
  (defmethod unparse-type ((tp emulated-llong)) :long-long)
  (defmethod canonicalize ((tp emulated-ullong)) :unsigned-long-long)
  (defmethod unparse-type ((tp emulated-ullong)) :unsigned-long-long)

  (defun %emulated-mem-ref-64 (ptr type offset)
    (let ((value #+big-endian
	    (+ (ash (mem-ref ptr :unsigned-long offset) 32)
	       (mem-ref ptr :unsigned-long (+ offset 4)))
	    #+little-endian
	    (+ (mem-ref ptr :unsigned-long offset)
	       (ash (mem-ref ptr :unsigned-long (+ offset 4)) 32))))
      (if (and (eq type :long-long) (logbitp 63 value))
          (lognot (logxor value #xFFFFFFFFFFFFFFFF))
          value)))
  (defun %emulated-mem-set-64 (value ptr type offset)
    (when (and (eq type :long-long) (minusp value))
      (setq value (lognot (logxor value #xFFFFFFFFFFFFFFFF))))
    (%mem-set (ldb (byte 32 0) value) ptr :unsigned-long
              #+big-endian (+ offset 4) #+little-endian offset)
    (%mem-set (ldb (byte 32 32) value) ptr :unsigned-long
              #+big-endian offset #+little-endian (+ offset 4))
    value))

;; problematic code
;; slot missing
;; #<UNION-SPEC id:"_2353" name:"" type:NIL> TYPE SLOT-VALUE NIL) [fast-method]

'((:|Union| :|id| "_2353" :|name| "" :|context| "_357" :|location| "f21:1295"
   :|file| "f21" :|line| "1295" :|members| "_2825 _2826" :|size| "64" :|align|
   "64"))

'((:|PointerType| :|id| "_1633" :|type| "_2519" :|size| "64" :|align| "64"))


;;; Example parses for tests

(defparameter *example-argument*
  '((:|Argument| :|type| "_2378" :|location| "f21:1395" :|file| "f21" :|line|
     "1395")))
