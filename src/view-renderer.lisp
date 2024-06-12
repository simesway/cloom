(defpackage :view-renderer
  (:use :common-lisp :wad-reader)
  (:export :view-renderer :view-renderer-init :update :view-renderer-close
	   :draw-vline :draw-sprite :draw-sprite-centered :set-background :window-dimensions
	   :SCREEN-W :SCREEN-H :SCREEN-H-W :SCREEN-H-H :SCREEN-DIST))

(in-package :view-renderer)


(defparameter SCREEN-W 320)
(defparameter SCREEN-H 300)
(defparameter SCREEN-H-W (floor (/ SCREEN-W 2)))
(defparameter SCREEN-H-H (floor (/ SCREEN-H 2)))
(defparameter SCREEN-DIST (/ SCREEN-H-W (tan (math:convert-angle :degrees player:H-FOV))))

(defconstant MAX_COLORS 256)

(defclass view-renderer ()
  ((window  :accessor window)
   (winptr  :accessor winptr)
   (width   :accessor width)
   (height  :accessor height)
   (asset-data :accessor asset-data)))


(defmethod window-dimensions (view-renderer)
  (let (width height)
    (charms/ll:getmaxyx (winptr view-renderer) height width)
    (values width height)))

(defun update-globals (width height)
  (setf SCREEN-W width)
  (setf SCREEN-H height)
  (setf SCREEN-H-W (floor (/ SCREEN-W 2)))
  (setf SCREEN-H-H (floor (/ SCREEN-H 2)))
  (setf SCREEN-DIST (/ SCREEN-H-W (tan (math:convert-angle :degrees player:H-FOV)))))

(defmethod update-window-dimensions (view-renderer)
  (with-slots (winptr width height) view-renderer
    (charms/ll:getmaxyx winptr height width)
    (update-globals width height)))

(defun scale (val min1 max1 min2 max2)
  (let ((scale1 (- max1 min1))
	(scale2 (- max2 min2)))
    (+ (* (/ (- val min1) scale1) scale2) min2)))

(defmacro define-scale-function (name min1 max1 min2 max2 round)
  `(defun ,name (value)
     (let ((new-val (scale value ,min1 ,max1 ,min2 ,max2)))
       (if ,round
	   (round new-val)
	   new-val))))

(define-scale-function scale-rgb-to-ncurses 0 255 0 1000 t)

(defun create-colors (palette)
  (dotimes (i MAX_COLORS)
    (with-slots (wad-types::r wad-types::g wad-types::b) (nth i palette)
      (let ((r (scale-rgb-to-ncurses wad-types::r))
	    (g (scale-rgb-to-ncurses wad-types::g))
	    (b (scale-rgb-to-ncurses wad-types::b)))
	(charms/ll:init-color i r g b)))))

(defun create-color-palette (palette font-color-index)
  (create-colors palette)
  (dotimes (i MAX_COLORS)
    (charms/ll:init-pair i font-color-index i)))
    
(defun load-color-palette (asset-data palette-index font-color-index)
  (with-slots (wad-reader) asset-data
  (let ((palette (wad-reader::get-color-palette wad-reader palette-index)))
    (create-color-palette palette font-color-index))))

(defun color-init (asset-data)
  (when (eql charms/ll:true (charms/ll:has-colors))
    (charms/ll:start-color)
    (load-color-palette asset-data 0 0)))

(defun view-renderer-init (asset-data-obj)
  (let ((renderer (make-instance 'view-renderer)))
    (with-slots (window winptr asset-data) renderer
      (setf window (charms:initialize))
      (setf winptr (charms::window-pointer window))
      (setf asset-data asset-data-obj)
      (charms:enable-non-blocking-mode window))
    (charms/ll:curs-set charms/ll:false)
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (color-init asset-data-obj)
    (update-window-dimensions renderer)
    renderer))

(defun view-renderer-close ()
  (charms/ll:getch)
  (charms:finalize))


(defmacro with-color ((winptr color-index) &body body)
  `(let ((index (mod ,color-index MAX_COLORS)))
     (charms/ll:wattron  ,winptr (charms/ll:color-pair ,color-index))
     ,@body
     (charms/ll:wattroff ,winptr (charms/ll:color-pair ,color-index))))

(defmacro defun-grayscale (name grayscale)
  `(defun ,name (lightlevel)
     (let ((index (truncate (* (/ lightlevel 256.0) ,(length grayscale)))))
       (elt ,grayscale index))))

(defun-grayscale short-grayscale1 "@#%+=*:-. ")
(defun-grayscale short-grayscale2 "$EFLlv!;,. ")
(defun-grayscale long-grayscale "$@B%8&WM#oahkbdpqwmZO0QLCJUYXzcvunxrjft/\|()1{}[]?-_+~<>i!lI;:,^`'. ")

(defun texture-to-random-color (texture-name seed)
  (abs (+ seed (sxhash texture-name))))

(defmethod draw-vline (view-renderer x y1 y2 texture lightlevel)
  (with-slots (window winptr) view-renderer
    (let ((color-id (mod (texture-to-random-color texture 5) MAX_COLORS))
	  (char     (short-grayscale1 (mod lightlevel 256))))
      (with-color (winptr color-id)
	(loop for y from y1 to y2
	      do (charms/ll:mvwaddch winptr y x (char-code char)))))))

(defmethod draw-color-char (view-renderer x y &optional (color-id 0) (char #\Space))
  (with-slots (winptr) view-renderer
    (with-color (winptr color-id)
      (charms/ll:mvwaddch winptr y x (char-code char)))))
    
(defmethod set-background (view-renderer color-id)
  (with-slots (winptr) view-renderer
    (with-color (winptr color-id)
      (charms/ll:wbkgd winptr color-id))))

(defmethod draw-sprite (view-renderer patch &optional (x 0) (y 0))
  (with-slots (asset-data::columns asset-data::width asset-data::height) patch
    (let ((columns asset-data::columns)
	  (width   asset-data::width)
	  (height  asset-data::height))
      (dolist (column columns)
	(let ((top-delta (wad-types::top-delta column))
	      (color-ids (wad-types::data column))
	      (length    (wad-types::num-pixel column)))
	  (if (eql top-delta #xFF)
	      (progn
		(setf x (1+ x)))
	      (loop for yi from 0 below length
		    do (let ((color-id (nth yi color-ids))
			     (yd (+ y yi top-delta)))
			 (draw-color-char view-renderer x yd color-id)))))))))

(defun draw-image (view-renderer image &key (x 0) (y 0) centered x-centered y-centered)
  (let* ((dimensions (array-dimensions image))
	 (img-w (nth 0 dimensions))
	 (img-h (nth 1 dimensions)))
    (if centered
	(progn (setf x-centered t)
	       (setf y-centered t)))
    (with-slots (width height) view-renderer
      (if x-centered
	  (setf x (+ x (truncate (/ (- width  img-w) 2)))))
      (if y-centered
	  (setf y (+ y (truncate (/ (- height img-h) 2))))))
    (dotimes (xi img-w)
      (dotimes (yi img-h)
	(let ((color (aref image xi yi)))
	  (if color
	      (draw-color-char view-renderer (+ x xi) (+ y yi) color)))))))

(defun draw-image2 (view-renderer image &key (x 0) (y 0) centered x-centered y-centered (pixel-mode nil))
  (let* ((dimensions (array-dimensions image))
	 (img-w (nth 0 dimensions))
	 (img-h (nth 1 dimensions))
	 (new-w img-w)
	 (new-h img-h)
	 (iterations 1)
	 (color-id 1))
    (case pixel-mode
      (:half-pixel
       (progn (setf new-h (/ img-h 2))
	      (if (eql (mod y 2) 1)
		  (setf color-id 0))
	      (setf y (floor (/ y 2)))))
      (:square-pixel
       (progn (setf new-w (* img-w 2))
	      (setf x (* x 2))
	      (setf iterations 2))))
    (if centered
	(progn (setf x-centered t)
	       (setf y-centered t)))
    (with-slots (width height) view-renderer
      (if x-centered
	  (setf x (+ x (truncate (/ (- width  new-w) 2)))))
      (if y-centered
	  (setf y (+ y (truncate (/ (- height new-h) 2))))))
    (loop for xi from 0 below img-w do
      (dotimes (yi img-h)
	(let ((color (aref image xi yi)))
	  (if color
	      (case pixel-mode
		(:half-pixel
		 (if (eql (mod yi 2) color-id)
		     (draw-color-char view-renderer (+ x xi) (+ y (truncate (/ yi 2))) color)))
		(:square-pixel
		 (dotimes (i 2)
		   (draw-color-char view-renderer (+ x i (* 2 xi)) (+ y yi) color)))
		(nil
		 (draw-color-char view-renderer (+ x xi) (+ y yi))))))))))

(defmethod draw-sprite-centered (view-renderer patch)
  (with-slots (asset-data::width asset-data::height) patch
    (with-slots (width height) view-renderer
      (let ((start-x (truncate (/ (- width asset-data::width) 2)))
	    (start-y (truncate (/ (- height asset-data::height) 2))))
	(draw-sprite view-renderer patch start-x start-y)))))

(defmethod write-str (view-renderer x y string &optional (color-id 3))
  (with-slots (winptr window) view-renderer
    (with-color (winptr color-id)
      (charms:write-string-at-point window string x y))))

(defmethod update (view-renderer)
  (charms:refresh-window (window view-renderer))
  (update-window-dimensions view-renderer))
