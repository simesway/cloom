(defpackage :math
  (:use :common-lisp)
  (:export :norm-angle :angle-to-point :convert-angle :distance :rotate-vector))

(in-package :math)


;;; Euclidean distance between two vertexes
(defun distance (from-vertex to-vertex)
  (sqrt (reduce #'+ (mapcar #'(lambda (x) (* x x)) (mapcar #'- to-vertex from-vertex)))))

;;; Macro for converting angles between degrees and radians
(defmacro convert-angle (&key degrees radians)
  (cond (degrees `(* ,degrees ,(/ pi 180)))
	(radians `(* ,radians ,(/ 180 pi)))))

;;; Function for calculating the outgoing angle from on vertex to another
(defun angle-to-point (from-vertex to-vertex)
  (let* ((delta (mapcar #'- to-vertex from-vertex))
	 (dx (first  delta))
	 (dy (second delta)))
    (norm-angle (convert-angle :radians (atan dy dx)))))

(defun norm-angle (angle)
  (mod angle 360))

;;; Function that returns a vector rotated by an angle
(defun rotate-vector (angle &key x y vec)
  (let* ((rad-a (convert-angle :degrees angle))
	 (cos-a (cos rad-a))
	 (sin-a (sin rad-a)))
    (list (- (* x cos-a) (* y sin-a)) (+ (* x sin-a) (* y cos-a)))))
