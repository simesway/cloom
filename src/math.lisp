(defpackage :math
  (:use :common-lisp)
  (:export :norm-angle :angle-to-point :convert-angle))

(in-package :math)


(defmacro convert-angle (&key degrees radians)
  (cond (degrees `(* ,degrees ,(/ pi 180)))
	(radians `(* ,radians ,(/ 180 pi)))))

(defun angle-to-point (from-vertex to-vertex)
  (let* ((delta (mapcar #'- to-vertex from-vertex))
	 (dx (first  delta))
	 (dy (second delta)))
    (norm-angle (convert-angle :radians (atan dy dx)))))

(defun norm-angle (angle)
  (mod angle 360))
