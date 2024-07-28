(defpackage :bsp
  (:use :common-lisp :map-data :utilities :player :math :seg-handler :view-renderer)
  (:export :bsp :make-bsp))

(in-package :bsp)


;; Vars storing elements rendered by the map-renderer (used for debugging)

(defvar *player* '())
(defvar *map-dims* '())
(defvar *nodes* '())
(defvar *visited-nodes* '())
(defvar *all-segs* '())
(defvar *ssector-segs* '())
(defvar *segs-in-fov* '())
(defvar *sides* '())

(defclass bsp ()
  ((seg-handler :accessor seg-handler)
   (nodes :accessor nodes)
   (ssectors :accessor ssectors)
   (segs     :accessor segs)
   (player   :accessor player)
   (root-node-id :accessor root-node-id)
   (is-traverse  :accessor is-traverse
		 :initform t)))

;;; Seg object holding additional information on screen-x1/-x2 and angle 
(defclass pov-seg ()
  ((seg   :accessor seg   :initarg :seg)
   (x1    :accessor x1    :initarg :x1)
   (x2    :accessor x2    :initarg :x2)
   (angle :accessor angle :initarg :angle)))

(defun pov-seg (seg x1 x2 angle)
  (make-instance 'pov-seg :seg seg :x1 x1 :x2 x2 :angle angle))

;;; Function to create a Binary Space Partitioning Object given map-data, player and view-renderer
(defun make-bsp (map-data player-obj view-renderer)
  (let* ((bsp (make-instance 'bsp)))
    (with-slots (seg-handler nodes ssectors segs player root-node-id) bsp
      (setf seg-handler (seg-handler::seg-handler-init map-data player-obj view-renderer bsp))
      (setf nodes (map-data::nodes map-data))
      (setf *nodes* nodes)
      (setf ssectors (map-data::ssectors map-data))
      (setf segs (map-data::segs map-data))
      (setf *all-segs* segs)
      (setf player player-obj)
      (setq *player* player)
      (setf root-node-id (1- (length nodes))))
    (reset-vars)
    (setf *map-dims* (map-data::get-dimensions map-data))
    bsp))

(defun reset-vars () ; for debugging with map-renderer
  (setf *segs-in-fov* '())
  (setf *visited-nodes* '())
  (setf *sides* '())
  (setf *ssector-segs* '()))

;;; Function that checks if a node is a leaf node (subsector)
(defun is-subsector (node-id)
  (logbitp 15 node-id))

;;; Function that checks if the player is on the left of the node's partition line
(defun is-on-left-side (player node)
  (with-slots (player::x player::y) player 
    (with-package-slots (x y dx dy) node
      (let* ((dx1 (- player::x x))
	     (dy1 (- player::y y)))
	(<= (- (* dx1 dy) (* dy1 dx)) 0)))))

;;; Function that checks if the bounding box of a (child) node is in the field of view
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

;;; Function that goes down the bsp to one or both sides (childs) of a partition line
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

;;; Function that maps an angle to the position on screen
(defun angle-to-x (angle)
  (let ((a (convert-angle :degrees angle)))
    (if (> angle 0)
	(floor (- SCREEN-DIST (*    (tan a) SCREEN-H-W)))
	(floor (+ SCREEN-DIST (* -1 (tan a) SCREEN-H-W))))))

;;; Function that calculates the location on screen of a segment and returns the pov-seg
(defun add-seg-to-fov (bsp seg)
  (with-package-slots (v1 v2) seg
    (let* ((vec1 (get-coordinates v1))
	   (vec2 (get-coordinates v2))
	   (p  (get-coordinates (player bsp)))
	   (a1 (angle-to-point  p vec1))
	   (a2 (angle-to-point  p vec2))
	   (span (norm-angle (- a1 a2))))

      ; Backface culling
      (if (>= span 180.0) (return-from add-seg-to-fov nil)) 
      
      (let* ((a-p (player::angle (player bsp)))
	     (a1-p (- a1 a-p))
	     (a2-p (- a2 a-p))
	     (span1 (norm-angle (+ a1-p player:H-FOV)))
	     (span2 (norm-angle (- player:H-FOV a2-p))))
	
	(if (> span1 player:FOV)
	    (if (>= span1 (+ span player:FOV))
		(return-from add-seg-to-fov nil)
		(setf a1-p player:H-FOV)))
	(if (> span2 player:FOV)
	    (if (>= span2 (+ span player:FOV))
		(return-from add-seg-to-fov nil)
		(setf a1-p (- player:H-FOV))))
	(let ((x1 (angle-to-x a1-p))
	      (x2 (angle-to-x a2-p)))
	  (pov-seg seg x1 x2 a1))))))

;;; Function that checks all segs making up a subsector
;;; and passes the ones in the player's fov to the seg-handler
(defun render-subsector (bsp subsector-id)
  (let* ((ssector (nth subsector-id (ssectors bsp)))
	 (segs    (segs bsp)))
    (with-package-slots (first-seg seg-count) ssector
      (dotimes (i seg-count)
	(let* ((seg     (nth (+ first-seg i) segs))
	       (pov-seg (add-seg-to-fov bsp seg)))
	  (if pov-seg
	      (progn (push seg *ssector-segs*)
		     (classify-segment (seg-handler bsp) pov-seg))))))))

;;; Function to start traversing and rendering the binary space tree given a node index
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

;;; Function that renders entire binary space tree
(defun render-bsp (bsp)
  (with-slots (root-node-id is-traverse) bsp
    (setf is-traverse t)
    (render-bsp-node bsp root-node-id)))

(defun get-subsector-height (bsp)
  (with-slots (root-node-id player nodes ssectors segs) bsp
    (let ((subsector-id root-node-id))
      (loop until (is-subsector subsector-id)
	    do (let ((node (nth subsector-id nodes)))
		 (with-package-slots (lchild rchild) node
		   (if (is-on-left-side player node)
		       (setf subsector-id lchild)
		       (setf subsector-id rchild))))
	    finally (return (let* ((ssector (nth (logand subsector-id #x7FFF) ssectors))
				   (seg     (nth (wad-types::first-seg ssector) segs))
				   (sector  (wad-types::fsector seg)))
			      (wad-types::floorheight sector)))))))

(defmethod seg-handler:get-player-height ((arg bsp))
  (+ player:HEIGHT (get-subsector-height arg)))

(defmethod seg-handler:stop-traversing-bsp ((arg bsp))
  (setf (is-traverse arg) nil))
