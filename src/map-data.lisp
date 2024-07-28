(defpackage :map-data
  (:use :common-lisp :wad-reader :utilities)
  (:export :map-data :map-data-init :get-thing-of-type))

(in-package :map-data)


;;;; 

(defclass map-data ()
  ((things   :accessor things)
   (linedefs :accessor linedefs)
   (sidedefs :accessor sidedefs)
   (vertexes :accessor vertexes)
   (segs     :accessor segs)
   (ssectors :accessor ssectors)
   (nodes    :accessor nodes)
   (sectors  :accessor sectors)))

;;; Macro that transferts a slot value to a map object 
(defmacro set-map-slot (slot-name get-function)
  `(setf (slot-value map ',slot-name) (,get-function wad-reader map-index)))

;;; Method that reads and stores all the data belonging to a map (index) to a map-data object
(defmethod get-map-data (wad-reader map-index)
  (let ((map (make-instance 'map-data)))
    (with-slots (things linedefs vertexes segs ssectors nodes sectors) map
      (set-map-slot things   get-things)
      (set-map-slot linedefs get-linedefs)
      (set-map-slot sidedefs get-sidedefs)
      (set-map-slot vertexes get-vertexes)
      (set-map-slot segs     get-segs)
      (set-map-slot ssectors get-ssectors)
      (set-map-slot nodes    get-nodes)
      (set-map-slot sectors  get-sectors))
    map))

;;; Method that resolves the empty front-/backside slot in the wad-types::linedef definition
(defmethod update-linedefs (map-data)
  (with-slots (linedefs sidedefs) map-data
    (dolist (linedef linedefs)
      (with-slots (wad-types::side-id1  wad-types::side-id2
		   wad-types::frontside wad-types::backside) linedef
	(setf wad-types::frontside (nth wad-types::side-id1 sidedefs))
	(let ((backside))
	  (if (not (eql wad-types::side-id2 #xFFFF)) ; Has backside if id2 != -1
	      (setf backside (nth wad-types::side-id2 sidedefs)))
	  (setf wad-types::backside backside))))))

;;; Method that resolves the empty sector slot in the wad-types::sidedef definition
(defmethod update-sidedefs (map-data)
  (with-slots (sidedefs sectors) map-data
    (dolist (sidedef sidedefs)
      (with-slots (wad-types::sec-id wad-types::sector) sidedef
	(setf wad-types::sector (nth wad-types::sec-id sectors))))))

;;; Function that converts the Binary Angular Measurement System (BAMS) to degrees
(defun bams-to-degrees (binary-angle)
  (let ((angle (* (ash binary-angle 16) 8.38190317e-8)))
    (if (< angle 0)
	(+ angle 360)
	angle)))

;;; Method that resolves the empty slots in the wad-types::seg definition
(defmethod update-segs (map-data)
  (with-slots (linedefs vertexes segs) map-data
    (dolist (seg segs)
      ; Dereference indices
      (with-slots (wad-types::v1-id wad-types::v2-id wad-types::line-id
		   wad-types::v1    wad-types::v2    wad-types::ldef) seg
	(setf wad-types::v1   (nth wad-types::v1-id   vertexes))
	(setf wad-types::v2   (nth wad-types::v2-id   vertexes))
	(setf wad-types::ldef (nth wad-types::line-id linedefs)))
      
      ; Add frontside-/backside-sector to segment
      (with-slots (wad-types::directn wad-types::fsector wad-types::bsector wad-types::ldef) seg
	(let ((front-sidedef)
	      (back-sidedef))
	  (with-slots (wad-types::frontside wad-types::backside) wad-types::ldef
	    (if (not (= 0 wad-types::directn)) ; Check for the orientation of the linedef
		(progn (setf front-sidedef wad-types::backside)
		       (setf back-sidedef  wad-types::frontside))
		(progn (setf front-sidedef wad-types::frontside)
		       (setf back-sidedef  wad-types::backside))))

	  ; Handle flag of the linedef
	  (setf wad-types::fsector (slot-value front-sidedef 'wad-types::sector))
	  (if (string= "TWO_SIDED" (wad-types:linedef-flag (slot-value wad-types::ldef 'wad-types::flags) :name))
	      (setf wad-types::bsector (slot-value back-sidedef 'wad-types::sector))
	      (setf wad-types::bsector nil))))

      ; Convert BAMS to degrees
      (with-slots (wad-types::angle) seg
	(setf wad-types::angle (bams-to-degrees wad-types::angle))))))

(defmethod update-data (map-data)
  (update-linedefs map-data)
  (update-sidedefs map-data)
  (update-segs     map-data))

;;; Method for reading and updating the map-data of a given map name
(defmethod map-data-init (filepath map-name)
  (let* ((reader (wad-reader-init filepath))
	 (map-id (get-lump-index reader map-name))
	 (map    (get-map-data   reader map-id)))
    (update-data map)
    (wad-reader-close reader)
    map))

;;; Function to return the Thing of a map given an index
(defun get-thing-of-type (map-data type-id)
  (with-slots (things) map-data
    (dolist (thing things)
      (when (eql (wad-types::t-type thing) type-id)
	(return-from get-thing-of-type thing)))))

;;; Functions used for getting the coordinate bounds of a map

(defun get-dimensions (map-data)
  (with-slots (vertexes) map-data
    (let* ((x-sorted (sort vertexes #'< :key 'wad-types::x))
	   (last     (1- (length vertexes)))
	   (x-min    (wad-types::x (nth 0    x-sorted)))
	   (x-max    (wad-types::x (nth last x-sorted)))
	   (y-sorted (sort vertexes #'< :key 'wad-types::y))
	   (y-min    (wad-types::y (nth 0    y-sorted)))
	   (y-max    (wad-types::y (nth last y-sorted))))
      (list (list x-min x-max) (list y-min y-max)))))

(defun get-dims (map-data)
  (with-slots (nodes) map-data
    (let* ((rboxes (mapcar (lambda (node) (slot-value node 'wad-types::rbox)) nodes))
	   (lboxes (mapcar (lambda (node) (slot-value node 'wad-types::lbox)) nodes))
	   (bboxes (append rboxes lboxes))
	   (x-min  (car (sort bboxes #'< :key 'wad-types::left)))
	   (x-max  (car (sort bboxes #'> :key 'wad-types::right)))
	   (y-min  (car (sort bboxes #'< :key 'wad-types::bottom)))
	   (y-max  (car (sort bboxes #'> :key 'wad-types::top))))
      (list (list x-min x-max) (list y-min y-max)))))
