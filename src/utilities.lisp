(defpackage :utilities
  (:use :common-lisp)
  (:export :with-package-slots :access-map-data :get-coordinates))

(in-package :utilities)


;;; Macros and Functions for reading slots without having to reference the package of origin

(defun pkg-sym (symbol package)
  (intern (symbol-name symbol) package))

(defmacro access-map-data (obj map-lump-type &optional n wad-type)
  (let ((wad-type-sym  (pkg-sym wad-type  'wad-types))
	(lump-type-sym (pkg-sym map-lump-type 'map-data)))
    (if n
	(if wad-type
	    `(slot-value (nth ,n (slot-value ,obj ',lump-type-sym)) ',wad-type-sym)
	    `(nth ,n (slot-value ,obj ',lump-type-sym)))
	`(slot-value ,obj ',lump-type-sym))))

(defun slot-value* (object slot-name)
  (let* ((package  (symbol-package (type-of object)))
	 (slot-sym (pkg-sym slot-name package)))
    (slot-value object slot-sym)))

(defmacro with-package-slots ((&rest slots) obj &body body)
  `(let ,(loop for slot in slots
	      collect `(,slot (slot-value* ,obj ',slot)))
     ,@body))

;;; Function for accessing the x & y coordinates of any object (that has these)
(defun get-coordinates (obj)
  (with-package-slots (x y) obj
    (list x y)))
