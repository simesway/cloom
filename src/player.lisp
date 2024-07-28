(defpackage :player
  (:use :common-lisp :map-data :utilities :math)
  (:export :player :player-init
	   :get-position :rotate-player :move-player
	   :FOV :H-FOV :HEIGHT))


(in-package :player)

;;;; Simple Functions and Class Definition for Player Loading and Handling


;;; Constants used for rendering the Player's view 

(defconstant FOV 90)
(defconstant H-FOV (/ FOV 2))
(defconstant HEIGHT 41) ; Height of Player

(defconstant PLAYER-SPEED 3)
(defconstant PLAYER-ROT-SPEED 2)

(defclass player ()
  ((x :accessor x)
   (y :accessor y)
   (angle :accessor angle)))

(defun player-init (map-data)
  (let ((thing (access-map-data map-data things 0)) ; First Thing is Player 0
	(player (make-instance 'player)))
    (with-slots (x y angle) player
      (setf x (wad-types::x thing))
      (setf y (wad-types::y thing))
      (setf angle (wad-types::angle thing)))
    player))

(defun get-position (player)
 (with-slots (x y) player
   (list x y)))

(defun move-player (player &key direction)
  (with-slots (x y angle) player
    (let ((inc-x 0)
	  (inc-y 0)
	  (speed PLAYER-SPEED))
      (case direction
	(:left  (incf inc-y speed))
	(:right (decf inc-y speed))
	(:forward  (incf inc-x speed))
	(:backward (decf inc-x speed)))
      (let ((inc (rotate-vector angle :x inc-x :y inc-y)))
	(incf x (first  inc))
	(incf y (second inc))))))

(defun rotate-player (player &key direction)
  (case direction
    (:right (incf (angle player) (- PLAYER-ROT-SPEED)))
    (:left  (incf (angle player)    PLAYER-ROT-SPEED))))
