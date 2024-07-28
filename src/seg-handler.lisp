(defpackage :seg-handler
  (:use :common-lisp :map-data :utilities :player :math :view-renderer)
  (:export :seg-handler :seg-handler-init :classify-segment :get-player-height :stop-traversing-bsp))

(in-package :seg-handler)

(defconstant MAX-SCALE 64.0)
(defconstant MIN-SCALE 0.000390625)

(defclass seg-handler ()
  ((bsp      :accessor bsp
             :initarg :bsp)
   (map-data :accessor map-data
	     :initarg :map-data)
   (player   :accessor player
	     :initarg :player)
   (renderer :accessor renderer
	     :initarg :renderer)
   (upper-clip :accessor upper-clip)
   (lower-clip :accessor lower-clip)
   (screen-rng :accessor screen-rng)
   (x-to-angle :accessor x-to-angle)))

(defun seg-handler-init (map-data player renderer bsp)
  (let* ((handler (make-instance 'seg-handler
				 :bsp      bsp
				 :map-data map-data
				 :player   player
				 :renderer renderer)))
    (reset-clipping handler)
    (setf (x-to-angle handler) (get-screen-angles))
    handler))

(defun reset-clipping (seg-handler)
  (clip-height-init seg-handler)
  (screen-range-init seg-handler))

;;; Method for initializing the clipping arrays
;;; Used for indicating the vertically rendered parts of the screen
(defmethod clip-height-init (seg-handler)
  (with-slots (upper-clip lower-clip) seg-handler
    (setf upper-clip (make-list SCREEN-W :initial-element -1))
    (setf lower-clip (make-list SCREEN-W :initial-element SCREEN-H))))

;;; Method for initializing the screen range array
;;; Used for indicating the horizontally rendered parts of the screen
(defmethod screen-range-init (seg-handler)
  (setf (screen-rng seg-handler)
	(loop for i from 0 below SCREEN-W collect i)))

;;; Function that calculates the in-game angle of each pixel column on screen
(defun get-screen-angles ()
  (let* (table)
    (loop for i from 0 upto SCREEN-W
	  do (setf table (cons (convert-angle :radians (atan (/ (- SCREEN-H-H i) SCREEN-DIST))) table)))
    (reverse table)))

(defun scale-from-global-angle (seg-handler x normal-angle distance)
  (let* ((x-angle (nth x (x-to-angle seg-handler)))
	 (p-angle (player::angle (player seg-handler)))
	 (den (* distance    (cos (convert-angle :degrees x-angle))))
	 (num (* SCREEN-DIST (cos (convert-angle :degrees (- normal-angle x-angle p-angle)))))
	 (scale (/ num den)))
    (min MAX-SCALE (max MIN-SCALE scale))))


;;; Function that renders the given fov-seg as a solid wall
(defun draw-solid-wall (seg-handler fov-seg x1 x2)
  (with-slots (bsp player renderer upper-clip lower-clip) seg-handler 
    (with-package-slots (v1 ldef fsector angle) (utilities::slot-value* fov-seg 'seg)
      (with-package-slots (floorheight ceilingheight lightlevel ceilingflat floorflat) fsector
	(let* ((side        (wad-types::frontside  ldef))
	       (texture     (wad-types::middle     side))
	       (p-height    (get-player-height bsp))
	       (z1 (- ceilingheight p-height))
	       (z2 (- floorheight   p-height))
	       (normal-a (+ angle 90))
	       (offset-a (- normal-a (utilities::slot-value* fov-seg 'angle)))
	       (hypoten  (distance (get-coordinates player) (get-coordinates v1)))
	       (dist     (* hypoten (cos (convert-angle :degrees offset-a))))
	       (scale1   (scale-from-global-angle seg-handler x1 normal-a dist))
	       (scale2   (scale-from-global-angle seg-handler x2 normal-a dist))
	       (scale-step (if (< x1 x2) (/ (- scale2 scale1) (- x2 x1)) 0))
	       ; Wall's vertical delimiters
	       (wall-y1      (- SCREEN-H-H (* z1 scale1)))
	       (wall-y1-step (* -1 scale-step z1))
	       (wall-y2      (- SCREEN-H-H (* z2 scale1)))
	       (wall-y2-step (* -1 scale-step z2))
	       draw-wall-y1 draw-wall-y2)

	  (if (not (renderer seg-handler))
	      (return-from draw-solid-wall))

	  (loop for x from x1 upto x2
		do (let ((draw-wall-y1 (1- wall-y1))
			 (draw-wall-y2     wall-y2))

		     (when (> z1 0 ) ; Draw ceiling 
		       (draw-flats seg-handler x (1- draw-wall-y1) ceilingflat lightlevel :side :ceiling))

		     (when (not (string= "-" texture)) ; Draw wall
		       (draw-wall seg-handler x draw-wall-y1 draw-wall-y2 texture lightlevel))

		     (when (< z2 0 ) ; Draw floor
		       (draw-flats seg-handler x (1+ draw-wall-y2) floorflat lightlevel :side :floor))

		     (incf wall-y1 wall-y1-step)
		     (incf wall-y2 wall-y2-step))))))))

(defun neql (x y) (not (eql x y)))

;;; Function that checks if two sectors have different properties 
(defun has-sector-diff (front-z back-z fsector bsector texture-sym)
  (let* ((f-ll (wad-types::lightlevel fsector))
	 (b-ll (wad-types::lightlevel bsector))
	 (f-tex (slot-value fsector texture-sym))
	 (b-tex (slot-value bsector texture-sym)))
    (or (neql front-z back-z) (neql f-ll b-ll) (neql f-tex b-tex))))

;;; Macros and Functions that draw the Walls, Floors and Ceilings

;;; Get y-limit considering the already rendered parts of the screen
(defmacro y-limit (seg-handler x &key (side :ceiling))
  (case side
    (:ceiling `(1+ (nth ,x (slot-value ,seg-handler 'upper-clip))))
    (:floor   `(1- (nth ,x (slot-value ,seg-handler 'lower-clip))))))

;;; Macro for handling y-limit on ceiling or floor 
(defmacro resolve-wall-y (seg-handler x y &key (clip-side :ceiling))
  (case clip-side
    (:ceiling `(truncate (max ,y (y-limit ,seg-handler ,x :side :ceiling))))
    (:floor   `(truncate (min ,y (y-limit ,seg-handler ,x :side :floor))))))

;;; Macro that evaluates the vertical rendering area of the wall, passing the properties to the view-renderer
(defun draw-wall (seg-handler x y1 y2 texture lightlevel)
  (let ((y1 (resolve-wall-y seg-handler x y1 :clip-side :ceiling))
	(y2 (resolve-wall-y seg-handler x y2 :clip-side :floor)))
    (view-renderer:draw-vline (renderer seg-handler) x y1 y2 texture lightlevel)))

;;; Function that draws the ceiling or floor through filling the area above or beneath the wall
(defun draw-flats (seg-handler x y texture lightlevel &key (side :ceiling))
  (let ((y1 (case side (:ceiling (y-limit        seg-handler x        :side :ceiling))
		       (:floor   (resolve-wall-y seg-handler x y :clip-side :floor))))
	(y2 (case side (:ceiling (resolve-wall-y seg-handler x y :clip-side :ceiling))
		       (:floor   (y-limit        seg-handler x        :side :floor)))))
    (view-renderer:draw-vline (renderer seg-handler) x y1 y2 texture lightlevel)))

;;; Function that draws a pov-seg as a portal wall
(defun draw-portal-wall (seg-handler pov-seg x1 x2)
  (with-slots (bsp screen-rng player) seg-handler
    (with-package-slots (v1 ldef fsector bsector angle) (utilities::slot-value* pov-seg 'seg)
      (let* ((frontside (wad-types::frontside ldef))
	     (p-height  (get-player-height bsp))
	     ; Calculate relative plane heights of front and back sector
	     (front-z1 (- (wad-types::ceilingheight fsector) p-height))
	     (back-z1  (- (wad-types::ceilingheight bsector) p-height))
	     (front-z2 (- (wad-types::floorheight   fsector) p-height))
	     (back-z2  (- (wad-types::floorheight   bsector) p-height))
	     has-upper-wall has-ceil has-lower-wall has-floor)
	(with-package-slots (upper lower) frontside

	  ; render upper wall and ceiling if difference in upper parts of front and backsector		    
	  (when (has-sector-diff front-z1 back-z1 fsector bsector 'wad-types::ceilingflat)
	    (setf has-upper-wall (and (neql "-" upper) (< back-z1 front-z1)))
	    (setf has-ceil       (>= front-z1 0))) ; upper wall edge is above players eyes

	  ; render lower wall and floor if difference in lower parts of front and backsector		    
	  (when (has-sector-diff front-z2 back-z2 fsector bsector 'wad-types::floorflat)
	    (setf has-lower-wall (and (neql "-" upper) (> back-z2 front-z2)))
	    (setf has-floor      (<= front-z2 0))) ; lower wall edge is beneath players eyes

	  ; return if nothing must be rendered
	  (when (and (not has-upper-wall) (not has-ceil) (not has-lower-wall) (not has-floor))
	    (return-from draw-portal-wall))

	  ; calculate scaling factors of the left and right edges of the wall range
	  (let* ((normal-a (+ angle 90))
		 (offset-a (- normal-a (utilities::slot-value* pov-seg 'angle)))
		 (hypoten  (distance (get-coordinates player) (get-coordinates v1)))
		 (dist     (* hypoten (cos (convert-angle :degrees offset-a))))
		 (scale1   (scale-from-global-angle seg-handler x1 normal-a dist))
		 (scale2   (scale-from-global-angle seg-handler x2 normal-a dist))
		 (scale-step (if (< x1 x2) (/ (- scale2 scale1) (- x2 x1)) 0))
		 (wall-y1      (- SCREEN-H-H (* front-z1 scale1)))
		 (wall-y1-step (* -1 scale-step front-z1))
		 (wall-y2      (- SCREEN-H-H (* front-z2 scale1)))
		 (wall-y2-step (* -1 scale-step front-z2))
		 (portal-y1      wall-y2)
		 (portal-y1-step wall-y2-step)
		 (portal-y2      wall-y1)
		 (portal-y2-step wall-y1-step))

	    ;; y-position of the top/bottom edge of the portal
	    
	    (when (and has-upper-wall (> back-z1 front-z2))
	      (setf portal-y1 (- SCREEN-H-H (* back-z1 scale1)))
	      (setf portal-y1-step (* -1 scale-step back-z1)))

	    (when (and has-lower-wall (< back-z2 front-z1))
	      (setf portal-y2 (- SCREEN-H-H (* back-z2 scale1)))
	      (setf portal-y2-step (* -1 scale-step back-z2)))

	    (if (not (renderer seg-handler)) (return-from draw-portal-wall))
	    
	    (with-package-slots (lower-clip upper-clip) seg-handler
	    (with-package-slots (floorflat ceilingflat lightlevel) fsector
	    ;; Rendering is carried out
	      (loop for x from x1 upto x2 do
		(let ((draw-wall-y1 (1- wall-y1))
		      (draw-wall-y2     wall-y2))

		  ;; Render each part and add the rendered areas to the clipping arrays
		  
		  (when has-upper-wall
		    (let* ((uy1 (1- wall-y1))
			   (uy2   portal-y1)
			   (wy2 (resolve-wall-y seg-handler x uy2 :clip-side :floor)))
		      (when has-ceil
			(draw-flats seg-handler x (1- uy1) ceilingflat lightlevel :side :ceiling))
		      (draw-wall seg-handler x uy1 uy2 upper lightlevel)

		      (if (< (nth x upper-clip) wy2)
			  (setf (nth x upper-clip) wy2)))
		    (incf portal-y1 portal-y1-step))
		  
		  (when has-ceil
		    (draw-flats seg-handler x draw-wall-y1 ceilingflat lightlevel :side :ceiling)
		    (let ((y2 (resolve-wall-y seg-handler x (1- draw-wall-y1) :clip-side :floor)))
		      (if (< (nth x upper-clip) y2)
			  (setf (nth x upper-clip) y2))))
		  
		  (when has-lower-wall
		    (when has-floor
		      (draw-flats seg-handler x draw-wall-y2 floorflat lightlevel :side :floor))
		    (let* ((y1 (1- portal-y2))
			   (y2       wall-y2)
			   (wy1 (resolve-wall-y seg-handler x y1 :clip-side :ceiling)))
		      (draw-wall seg-handler x y1 y2 lower lightlevel)
		      (if (> (nth x lower-clip) wy1)
			  (setf (nth x lower-clip) wy1)))
		    (incf portal-y2 portal-y2-step))

		  (when has-floor
		    (draw-flats seg-handler x (1+ draw-wall-y2) floorflat lightlevel :side :floor)
		    (let ((y1 (resolve-wall-y seg-handler x (1+ draw-wall-y2) :clip-side :ceiling)))
		      (if (> (nth x lower-clip) (1+ draw-wall-y2))
			  (setf (nth x lower-clip) y1)))))
		(incf wall-y1 wall-y1-step)
		(incf wall-y2 wall-y2-step))))))))))
		    
(defmacro setf-if (function obj val)
  `(if (apply ,function ,obj (list ,val))
       (setf ,obj ,val)))

(defun remove-elements (elements from-list)
  (remove-if #'(lambda (x) (member x elements)) from-list))

;;; Function that checks if the solid wall lies in area in screen range that has not been rendered yet 
;;; and passes it to the rendering functions
(defun clip-solid-wall (seg-handler pov-seg)
  (with-slots (bsp screen-rng) seg-handler
    (when (not screen-rng)
      (stop-traversing-bsp bsp)
      (return-from clip-solid-wall))
    
    ; what area has to be drawn
    (with-package-slots (seg x1 x2) pov-seg
      (let* ((curr-wall (loop for i from x1 below x2 collect i))
	     (intersect (intersection curr-wall screen-rng)))
	(when (not intersect)
	  (return-from clip-solid-wall))

	(if (eql (length intersect) (length curr-wall))
	    (draw-solid-wall seg-handler pov-seg x1 (1- x2))
	    (let* ((arr (sort intersect #'<))
		   (x      (first arr))
		   (x2 (car (last arr))))
	      (loop for x1 in arr
		    for x2 in (cdr arr)
		    do (when (> (- x2 x1) 1)
			 (draw-solid-wall seg-handler pov-seg x x1)
			 (setf x x2)))
	      (draw-solid-wall seg-handler pov-seg x x2)))
	(setf screen-rng (remove-elements intersect screen-rng))))))

;;; Function that checks if the portal wall lies in area in screen range that has not been rendered yet 
;;; and passes it to the rendering functions
(defun clip-portal-wall (seg-handler pov-seg)
  (with-slots (bsp screen-rng) seg-handler
    ; what area has to be drawn
    (with-package-slots (seg x1 x2) pov-seg
      (let* ((curr-wall (loop for i from x1 below x2 collect i))
	     (intersect (intersection curr-wall screen-rng)))
	(when (not intersect)
	  (return-from clip-portal-wall))
	(if (eql (length intersect) (length curr-wall))
	    (draw-portal-wall seg-handler pov-seg x1 (1- x2))
	    (let* ((sorted (sort intersect #'<))
		   (x      (first sorted))
		   (x2 (car (last sorted))))
	      (loop for x1 in sorted
		    for x2 in (cdr sorted)
		    do (when (> (- x2 x1) 1)
			 (draw-portal-wall seg-handler pov-seg x x1)
			 (setf x x2)))
	      (draw-portal-wall seg-handler pov-seg x x2)))))))

;;; Checks if a seg has no sector on the back
(defun is-solid-wall (seg)
  (null (wad-types::bsector seg)))

;;; Macro that compares the same slot from two different objects
(defmacro compare-slots (slot-name object1 object2 &optional (function #'eql))
  `(,function (slot-value ,object1 ,slot-name) (slot-value ,object2 ,slot-name)))

;;; Checks if a seg is a portal wall, where front and back sector differ in heights
(defun is-with-window (seg)
  (with-package-slots (fsector bsector) seg
    (or (neql (wad-types::ceilingheight fsector) (wad-types::ceilingheight bsector))
	(neql (wad-types::floorheight   fsector) (wad-types::floorheight   bsector)))))

;;; Reject empty lines used for triggers and events
(defun is-special-type (seg)
  (with-package-slots (fsector bsector ldef) seg
    (and (string= (wad-types::ceilingflat fsector) (wad-types::ceilingflat bsector))
	 (eql     (wad-types::lightlevel  fsector) (wad-types::lightlevel  bsector))
	 (string= (wad-types::floorflat   fsector) (wad-types::floorflat   bsector))
	 (string= (wad-types::middle (wad-types::frontside ldef)) "-"))))

;;; Function that receives the pov-seg from the bsp::render-subsector function 
;;; and handles it according to it's type of wall
(defun classify-segment (seg-handler pov-seg)
  (with-package-slots   (seg x1 x2)   pov-seg
    (cond ((eql x1 x2)           (return-from classify-segment))
	  ((is-solid-wall   seg) (clip-solid-wall seg-handler pov-seg))
	  ((is-with-window  seg) (clip-portal-wall seg-handler pov-seg))
	  ((is-special-type seg) (return-from classify-segment))
	  (t                     (clip-portal-wall seg-handler pov-seg)))))

(defgeneric get-player-height (arg))
(defgeneric stop-traversing-bsp (arg))
