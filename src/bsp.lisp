(defpackage :bsp
  (:use :common-lisp :map-data :utilities :player :math)
  (:export :bsp :make-bsp))

(in-package :bsp)


(defvar *player* '())
(defvar *map-dims* '())
(defvar *nodes* '())
(defvar *visited-nodes* '())
(defvar *all-segs* '())
(defvar *ssector-segs* '())
(defvar *segs-in-fov* '())
(defvar *sides* '())

(defclass bsp ()
  ((nodes :accessor nodes)
   (ssectors :accessor ssectors)
   (segs     :accessor segs)
   (player   :accessor player)
   (root-node-id :accessor root-node-id)
   (is-traverse  :accessor is-traverse
		 :initform t)))

(defun make-bsp (map-data player-obj)
  (let* ((bsp (make-instance 'bsp)))
    (with-slots (nodes ssectors segs player root-node-id) bsp
      (setf nodes (map-data::nodes map-data))
      (setf *nodes* nodes)
      (setf ssectors (map-data::ssectors map-data))
      (setf segs (map-data::segs map-data))
      (setf *all-segs* segs)
      (setf player player-obj)
      (setq *player* player)
      (setf root-node-id (1- (length nodes))))
    (setf *segs-in-fov* '())
    (setf *visited-nodes* '())
    (setf *sides* '())
    (setf *ssector-segs* '())
    (setf *map-dims* (map-data::get-dimensions map-data))
    bsp))

(defun is-subsector (node-id)
  (logbitp 15 node-id))

(defun is-on-left-side (player node)
  (with-slots (player::x player::y) player 
    (with-package-slots (x y dx dy) node
      (let* ((dx1 (- player::x x))
	     (dy1 (- player::y y)))
	(<= (- (* dx1 dy) (* dy1 dx)) 0)))))

(defun bbox-in-fov (bbox player)
  (with-slots (player::x player::y player::angle) player
    (with-package-slots (top bottom left right) bbox
      (let ((d (list left top))    (c (list right top))
	    (a (list left bottom)) (b (list right bottom))
	    (sides '()))
	(cond ((< player::x left)
	       (progn
		 (push (list d a) sides)
		 (cond ((> player::y top)    (push (list c d) sides))
		       ((< player::y bottom) (push (list a b) sides)))))
	      
	      ((< right player::x)
	       (progn
		 (push (list b c) sides)
		 (cond ((> player::y top)    (push (list c d) sides))
		       ((< player::y bottom) (push (list a b) sides)))))
	      
	      (t (cond ((> player::y top)    (push (list c d) sides))
		       ((< player::y bottom) (push (list a b) sides))
		       (t (return-from bbox-in-fov t)))))

	(dolist (side sides)
	  (let* ((p  (list player::x player::y))
		 (v1 (first side))
		 (v2 (second side))
		 (a1 (angle-to-point p v1))
		 (a2 (angle-to-point p v2))
		 (span (norm-angle (- a1 a2)))
		 (a1-p (- a1 player::angle))
		 (span-p (norm-angle (+ a1-p player:H-FOV))))
	    (if (or (not (> span-p player:FOV))
		    (not (>= span-p (+ span player:FOV))))
		(return-from bbox-in-fov t))))
	(return-from bbox-in-fov nil)))))


(defun render-child (bsp node side)
  (with-package-slots (lbox lchild rbox rchild) node
    (case side
      (:left
       (progn
	 (render-bsp-node bsp lchild)
	 (if (bbox-in-fov rbox (player bsp))
	     (render-bsp-node bsp rchild))))
      (:right
       (progn
	 (render-bsp-node bsp rchild)
	 (if (bbox-in-fov lbox (player bsp))
	     (render-bsp-node bsp lchild)))))))

(defun render-subsector (bsp subsector-id)
  (let* ((ssector (nth subsector-id (ssectors bsp)))
	 (segs    (segs bsp)))
    (with-package-slots (first-seg seg-count) ssector
      (dotimes (i seg-count)
	(let ((seg (nth (+ first-seg i) segs)))
	  (push seg *ssector-segs*))))))

(defun render-bsp-node (bsp node-id)
  (when (is-traverse bsp)
    (if (is-subsector node-id)
	(render-subsector bsp (logand node-id #x7FFF))
	(let* ((node (nth node-id (nodes bsp)))
	       (player (player bsp)))
	  (push node *visited-nodes*)
	  (if (is-on-left-side player node)
	      (render-child bsp node :left)
	      (render-child bsp node :right))))))
