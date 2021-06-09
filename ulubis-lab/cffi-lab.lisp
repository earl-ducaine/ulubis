

(in-package :ulubis-lab)

(defun test-window-ptr ()
  (with-foreign-objects ((window-ptr-sap '(:struct window) 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-ptr-sap
				     :cffi-type '(:struct window))))
      (values
       (convert-to-foreign window-ptr 'window-struct)
       (convert-from-foreign
       	(convert-to-foreign window-ptr 'window-struct)
       	'(:struct window))))))



(defun test-geometry-ptr ()
  (with-foreign-objects ((window-ptr-sap '(:struct window) 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-ptr-sap
				     :cffi-type '(:struct window))))
      (foreign-slot-value window-ptr-sap '(:struct window) 'geometry))))


(defun test-with-foreign-slots ()
  (with-foreign-objects ((window-ptr-sap '(:struct window) 1))
    (with-foreign-slots ((geometry benchmark-time frames fullscreen maximized)
			 window-ptr-sap
			 (:struct window))
      (format t "geometry(~s) benchmark-time(~s) ~%" geometry benchmark-time)
      (setf (getf geometry 'height) 250)
      (format t "geometry(~s) benchmark-time(~s) ~%" geometry benchmark-time))))


(defun test-agregate-slot ()
  (with-foreign-objects ((geometry-sap '(:struct geometry) 1)
			 (geometry-alt-sap '(:struct geometry) 1))
    ;; (setf (foreign-slot-value geometry-alt-sap '(:struct geometry) 'width) 5)
    ;; (setf (foreign-slot-value geometry-alt-sap '(:struct geometry) 'height) 5)
    (setf (mem-aref geometry-alt-sap '(:struct geometry) 0) '(HEIGHT 5 WIDTH 10))
    (setf (mem-aref geometry-sap '(:struct geometry) 0)
	  (mem-aref geometry-alt-sap '(:struct geometry) 0))
    (foreign-slot-value geometry-sap '(:struct geometry) 'width)))

(defun test-> ()
  (with-foreign-objects ((geometry-sap '(:struct geometry) 1)
			 (window-sap '(:struct window) 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-sap
				     :cffi-type '(:struct window)))
	  (geometry-ptr (make-instance 'geometry
				       :sap geometry-sap
				       :cffi-type '(:struct geometry))))
      (setf (-> geometry-ptr 'width) 5)
      (setf (-> geometry-ptr 'height) 10)
      (setf (-> window-ptr 'geometry) geometry-ptr)
      (-> (-> window-ptr 'geometry) 'width))))
    ;; (setf (foreign-slot-value geometry-alt-sap '(:struct geometry) 'width) 5)
    ;; (setf (foreign-slot-value geometry-alt-sap '(:struct geometry) 'height) 5)
    ;; (setf (mem-aref geometry-alt-sap '(:struct geometry) 0) '(HEIGHT 5 WIDTH 10))
    ;; (setf (mem-aref geometry-sap '(:struct geometry) 0)
    ;; 	  (mem-aref geometry-alt-sap '(:struct geometry) 0))
    ;; (foreign-slot-value geometry-sap '(:struct geometry) 'width)))


(defun test-bool ()
  (with-foreign-objects ((window-sap '(:struct window) 1))
    (let ((window-ptr (make-instance 'window
				     :sap window-sap
				     :cffi-type '(:struct window))))
      (setf (-> window-ptr 'wait-for-configure) nil)
      (-> window-ptr 'wait-for-configure))))
