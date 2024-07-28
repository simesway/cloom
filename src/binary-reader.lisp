(defpackage :binary-reader
  (:use :common-lisp)
  (:export :read-value :define-binary-type :define-binary-element-class))

(in-package :binary-reader)

;;; Functions and Macro Definitions for reading binary files


;;; Macro to generate unique symbols for temporary variables
(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect `(,var (gensym)))
     ,@body))

;;; Convert a symbol to a keyword
(defun as-keyword (sym) (intern (string sym) :keyword))

;;; Transform a slot specification into a defclass slot definition
(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

;;; Generic function for reading a value (element) from a binary stream
(defgeneric read-value (type stream &key))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

;;; Generate code to read a value (element) for a given slot from a binary stream
(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

;;; Macro to define a binary element class with specified slots
(defmacro define-binary-element-class (name slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ()
	 ,(mapcar #'slot->defclass-slot slots))

       (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	 (let ((,objectvar (make-instance ',name)))
	   (with-slots ,(mapcar #'first slots) ,objectvar
	     ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
	   ,objectvar)))))

;;; Macro to define a binary type with a custom read-value method
(defmacro define-binary-type (name stream (&rest args) &body spec)
  (with-gensyms (type)
    `(defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
       ,@spec)))
