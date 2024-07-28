(defpackage :wad-types
  (:use :common-lisp :binary-reader)
  (:export :*map-lumps* :int16 :uint8 :uint16 :uint32
           :ascii-string :ascii-4 :ascii-8
           :binary-element-list :bbox :color
	   :wadinfo :filelump :map-data
	   :patch-header :patch-column :patch-map
	   :texture-header :texture-map
           :thing :linedef :sidedef :vertex :seg :subsector :node :sector
	   :linedef-flag))

(in-package :wad-types)


;;; Order of map data lumps
(defconstant *map-lumps* (list "THINGS" "LINEDEFS" "SIDEDEFS" "VERTEXES" "SEGS"
			       "SSECTORS" "NODES" "SECTORS" "REJECT" "BLOCKMAP"))

;;; Constant for mapping linedef flags to ids
(defconstant *linedef-flags* (list "BLOCKING" "BLOCK_MONSTERS" "TWO_SIDED" "DONT_PEG_TOP"
				   "DONT_PEG_BOTTOM" "SECRET" "SOUND_BLOCK" "DONT_DRAW" "MAPPED"))

;;; Macro for converting linedef flags to string, id
(defmacro linedef-flag (flag return-value)
  (case return-value
    (:id   `(expt 2 (position ,flag *linedef-flags* :test #'string=)))
    (:name `(nth (truncate (if (eql 0 ,flag) 0 (log ,flag 2))) *linedef-flags*))))



;;;; ---- BINARY-TYPES ----
;;;; Binary type definitions for correctly reading DOOM's WAD-files

;;; Placeholder for empty binary-class slots 
(define-binary-type empty in ())

;;; Base type for reading integers
(define-binary-type generic-int in (bytes unsigned)
  (let ((value 0))
    (dotimes (i bytes)
      (setf value (logior (ash (read-byte in) (* i 8)) value)))
    (if (or unsigned (< value (ash 1 (1- (* bytes 8)))))
	value
	(- value (ash 1 (* bytes 8))))))

(define-binary-type  int16 in () (read-value 'generic-int in :bytes 2 :unsigned nil))
(define-binary-type uint8  in () (read-value 'generic-int in :bytes 1 :unsigned t))
(define-binary-type uint16 in () (read-value 'generic-int in :bytes 2 :unsigned t))
(define-binary-type uint32 in () (read-value 'generic-int in :bytes 4 :unsigned t))

;;; Type for reading lists of specific element-type
(define-binary-type binary-element-list in (length (element-type 'uint8))
  (let ((list '()))
    (dotimes (i length)
      (setf list (cons (read-value element-type in) list))) 
    (setf list (nreverse list))))

;;; Type for reading ascii strings
(define-binary-type ascii-string in (length)
  (with-output-to-string (s)
    (dotimes (i length)
      (let ((code (read-byte in)))
	(if (not (zerop code))
	    (write-char (code-char code) s))))))

(define-binary-type ascii-4 in () (read-value 'ascii-string in :length 4))
(define-binary-type ascii-8 in () (read-value 'ascii-string in :length 8))

;;; Binary classes for reading bounding boxes and rgb-colors

(define-binary-element-class bbox
  ((top    (int16))
   (bottom (int16))
   (left   (int16))
   (right  (int16))))

(define-binary-element-class color
  ((r (uint8))
   (g (uint8))
   (b (uint8))))



;;;; ---- WAD-READER ----

;;; Class for reading the header of a WAD file
(define-binary-element-class wadinfo
  ((identification (ascii-4))
   (numlumps        (uint32))
   (infotableofs    (uint32))))

;;; Class for reading a lump (directory entry for a data block)
(define-binary-element-class filelump
  ((filepos (uint32))
   (size    (uint32))
   (name   (ascii-8))))



;;;; ---- MAP-DATA ----

(define-binary-element-class thing
  ((x      (int16))
   (y      (int16))
   (angle  (int16))
   (t-type (int16))
   (flags  (int16))))

(define-binary-element-class linedef
  ((v1       (uint16))
   (v2       (uint16))
   (flags    (uint16))
   (special_t (int16))
   (tag       (int16))
   (side-id1 (uint16))
   (side-id2 (uint16))
   (frontside (empty))
   (backside  (empty))))

(define-binary-element-class sidedef
  ((xoffs  (int16))
   (yoffs  (int16))
   (upper  (ascii-8))
   (lower  (ascii-8))
   (middle (ascii-8))
   (sec-id (int16))
   (sector (empty))))

(define-binary-element-class vertex
  ((x (int16))
   (y (int16))))

(define-binary-element-class seg
  ((v1-id   (int16))
   (v2-id   (int16))
   (angle   (int16))
   (line-id (int16))
   (directn (int16))
   (offs    (int16))
   (v1      (empty))
   (v2      (empty))
   (ldef    (empty))
   (fsector (empty))
   (bsector (empty))))

(define-binary-element-class subsector
  ((seg-count (uint16))
   (first-seg (uint16))))

(define-binary-element-class node
  ((x      (int16))
   (y      (int16))
   (dx     (int16))
   (dy     (int16))
   (rbox   (bbox))
   (lbox   (bbox))
   (rchild (int16))
   (lchild (int16))))

(define-binary-element-class sector
  ((floorheight   (int16))
   (ceilingheight (int16))
   (floorflat   (ascii-8))
   (ceilingflat (ascii-8))
   (lightlevel    (int16))
   (s-type        (int16))
   (tag           (int16))))



;;;; ---- ASSET-DATA ----
;;;; Class definitions for reading Patches and Textures

(define-binary-element-class patch-header
  ((width    (uint16))
   (height   (uint16))
   (leftofs   (int16))
   (topofs    (int16))
   (columnofs (empty))
   (filepos   (empty))))

(define-binary-element-class patch-column
  ((top-delta (uint8))
   (num-pixel (empty))
   (pad-pre   (empty))
   (data      (empty))
   (pad-post  (empty))))

(define-binary-element-class texture-header
  ((num-tex (uint32))
   (offset   (empty))
   (dataofs  (empty))))

(define-binary-element-class patch-map
  ((x-ofs    (int16))
   (y-ofs    (int16))
   (patch   (uint16))
   (stepdir (uint16))    ;unused
   (color-m (uint16))))  ;unused

(define-binary-element-class texture-map
  ((name   (ascii-8))
   (masked  (uint32))
   (width   (uint16))
   (height  (uint16))
   (coldir  (uint32)) ;unused, obsolete
   (p-count (uint16))
   (patch-maps (empty))))
