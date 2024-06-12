(defpackage :map-renderer
  (:use :common-lisp :bsp :utilities :wad-types)
  (:export :map-renderer :add-seg))

(in-package :map-renderer)


(defvar *canvas-width*  800)
(defvar *canvas-height* 600)

(defvar *map-dims* '())
(defvar *x-min* 0)
(defvar *x-max* 0)
(defvar *y-min* 0)
(defvar *y-max* 0)

(defvar *iterator* 0)

(defvar *red*   (gamekit:vec4 1 0 0 1))
(defvar *green* (gamekit:vec4 0 1 0 1))
(defvar *blue*  (gamekit:vec4 0 0 1 1))
(defvar *magenta* (gamekit:vec4 1 0 1 1))
(defvar *yellow*  (gamekit:vec4 1 1 0 1))
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *gray* (gamekit:vec4 0.5 0.5 0.5 1))
(defvar *white* (gamekit:vec4 1 1 1 1))


(gamekit:defgame map-renderer () ()
  (:viewport-width  *canvas-width*)
  (:viewport-height *canvas-height*))

(defun set-map-dims ()
  (setq *map-dims* bsp::*map-dims*)
  (let ((x (first *map-dims*))
	(y (second *map-dims*)))
    (setq *x-min* (first  x))
    (setq *x-max* (second x))
    (setq *y-min* (first  y))
    (setq *y-max* (second y))))

(defun set-canvas-dims (width height)
  (setq *canvas-width* width)
  (setq *canvas-height* height))

(defun draw-bsp (&optional (width 800) (height 600))
  (set-map-dims)
  (set-canvas-dims width height)
  (format t "~a~%" *map-dims*)
  (format t "all-segs: ~a~%" (length bsp::*all-segs*))
  (format t "player: ~a~%" (player::angle bsp::*player*))
  (format t "vis-segs: ~a~%" (length bsp::*segs-in-fov*))
  (gamekit:start 'map-renderer)
  (sleep 15)
  (gamekit:stop))

(defun scale-x (x &optional (out-min 30) (out-max (- *canvas-width* 30)))
  (let ((scale (min (/ *canvas-width* (- *x-max* *x-min*))
		    (/ *canvas-height* (- *y-max* *y-min*)))))
    (+ (* (- x *x-min*) scale) 0)))

(defun scale-y (y &optional (out-min 30) (out-max (- *canvas-height* 30)))
  (let ((scale (min (/ *canvas-width* (- *x-max* *x-min*))
		    (/ *canvas-height* (- *y-max* *y-min*)))))
    (+ (* (- y *y-min*) scale) 0)))

(defun scale (&key x y)
  (cond (x (* 4 (/ (- x -100 (first (first *map-dims*))) *map-width*) *canvas-width*))
	(y (* 4 (/ (- y -100 (second (first *map-dims*)) *map-height*) *canvas-height*)))))

(defun vertex-to-vec2 (vertex)
  (with-package-slots (x y) vertex
    (point-to-vec2 x y)))

(defun point-to-vec2 (x y)
  (gamekit:vec2 (scale-x x) (scale-y y)))

(defun draw-bbox (bbox color)
  (with-package-slots (top bottom left right) bbox
    (let ((origin (point-to-vec2 left bottom))
	  (width  (- (scale-x right) (scale-x left)))
	  (height (- (scale-y top) (scale-y bottom))))
      (gamekit:draw-rect origin width height
			 :stroke-paint color :thickness 2))))

(defun draw-seg (segment color thickness)
  (with-package-slots (v1 v2) segment
    (let ((vec1 (vertex-to-vec2 v1))
	  (vec2 (vertex-to-vec2 v2)))
      (gamekit:draw-line vec1 vec2 color :thickness thickness))))

(defun draw-segs (segs color thickness)
  (dolist (elem segs)
    (if elem
	(draw-seg elem color thickness))))

(defun draw-node (node)
  (with-package-slots (rbox lbox) node
    (draw-bbox rbox *red*)
    (draw-bbox lbox *green*)))

(defun draw-nodes (nodes)
    (dolist (node nodes)
      (if node
	  (draw-node node))))

(defun draw-side (side)
  (let* ((v1 (first side))
	 (v2 (second side))
	 (vec1 (point-to-vec2 (first v1) (second v1)))
	 (vec2 (point-to-vec2 (first v2) (second v2))))
    (gamekit:draw-line vec1 vec2 *magenta* :thickness 2)))

(defun draw-sides (sides)
  (dolist (side sides)
    (draw-side side)))

(defun draw-player ()
  (with-package-slots (x y angle) bsp::*player*
    (let ((center (point-to-vec2 x y)))
      (gamekit:draw-circle center 5 :fill-paint *black*)
    (let* ((h-fov player::H-FOV)
	   (sin-a1 (sin (math::convert-angle :degrees (- angle h-fov))))
	   (cos-a1 (cos (math::convert-angle :degrees (- angle h-fov))))
	   (sin-a2 (sin (math::convert-angle :degrees (+ angle h-fov))))
	   (cos-a2 (cos (math::convert-angle :degrees (+ angle h-fov))))
	   (length (* 10 *canvas-height*)))
      (let* ((x1 (+ x (* length cos-a1)))
	     (y1 (+ y (* length sin-a1)))
	     (x2 (+ x (* length cos-a2)))
	     (y2 (+ y (* length sin-a2)))
	     (vec1 (point-to-vec2 x1 y1))
	     (vec2 (point-to-vec2 x2 y2)))
	(gamekit:draw-line center vec1 *yellow* :thickness 3)
	(gamekit:draw-line center vec2 *yellow* :thickness 3))))))

 (defmethod gamekit:draw ((this map-renderer))
   (draw-segs bsp::*all-segs* *black* 2)
   (draw-player)
   (draw-nodes bsp::*visited-nodes*)
   (draw-sides bsp::*sides*)
   (draw-segs bsp::*ssector-segs* *blue* 3))
