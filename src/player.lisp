(defpackage :player
  (:use :common-lisp :map-data :utilities)
  (:export :player :player-init
	   :get-position
	   :FOV :H-FOV))


(in-package :player)


(defconstant FOV 90)
(defconstant H-FOV (/ FOV 2))

(defclass player ()
  ((x :accessor x)
   (y :accessor y)
   (angle :accessor angle)))

(defun player-init (map-data)
  (let ((thing (access-map-data map-data things 0))
	(player (make-instance 'player)))
    (with-slots (x y angle) player
      (setf x (wad-types::x thing))
      (setf y (wad-types::y thing))
      (setf angle (wad-types::angle thing)))
    player))

(defun get-position (player)
 (with-slots (x y) player
    (list x y)))
