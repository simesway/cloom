(defpackage :math
  (:use :common-lisp)
  (:export :norm-angle :angle-to-point :convert-angle :distance :rotate-vector))

(in-package :math)


(defun distance (from-vertex to-vertex)
  (sqrt (reduce #'+ (mapcar #'(lambda (x) (* x x)) (mapcar #'- to-vertex from-vertex)))))

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

(defun rotate-vector (angle &key x y vec)
  (let* ((rad-a (convert-angle :degrees angle))
	 (cos-a (cos rad-a))
	 (sin-a (sin rad-a)))
    (list (- (* x cos-a) (* y sin-a)) (+ (* x sin-a) (* y cos-a)))))
