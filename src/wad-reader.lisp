(defpackage :wad-reader
  (:use :common-lisp :binary-reader :wad-types)
  (:export :wad-reader :wad-reader-init :get-map-data :wad-reader-close
	   :get-things :get-linedefs :get-sidedefs :get-vertexes :get-segs :get-ssectors :get-nodes :get-sectors
	   :get-lump-index :get-lumps :get-color-palette
	   :read-patch-header   :get-patch-column :get-pnames :get-flat-data
           :read-texture-header :get-texture-map))

(in-package :wad-reader)


(defclass wad-reader ()
  ((filepath
    :initarg  :filepath
    :accessor filepath)
   (file           :accessor file)
   (identification :accessor identification)
   (numlumps       :accessor numlumps)
   (infotableofs   :accessor infotableofs)
   (directory      :accessor wad-directory)))


(defmacro transfer-slot-values (slot-name from-object to-object)
  `(setf (slot-value ,to-object ',slot-name)
	 (slot-value ,from-object ',slot-name)))

(defmethod open-wadfile ((reader wad-reader))
  (setf (file reader) (open (filepath reader) :element-type '(unsigned-byte 8))))

(defmethod read-wadinfo ((reader wad-reader))
  (with-slots (file) reader
    (file-position file 0)
    (let ((wadinfo (read-value 'wadinfo file)))
      (setf (identification reader) (wad-types::identification wadinfo))
      (setf (numlumps       reader) (wad-types::numlumps       wadinfo))
      (setf (infotableofs   reader) (wad-types::infotableofs   wadinfo)))))

(defmethod read-directory ((reader wad-reader))
  (with-slots (file numlumps infotableofs directory) reader
    (file-position file infotableofs)
    (setf directory (read-value 'binary-element-list file
				:element-type 'filelump
				:length numlumps))))

(defmethod wad-reader-init (path)
  (let ((reader (make-instance 'wad-reader :filepath path)))
    (open-wadfile   reader)
    (read-wadinfo   reader)
    (read-directory reader)
    reader))

(defmethod wad-reader-close (wad-reader)
  (close (file wad-reader)))

(defmethod read-patch-header (wad-reader patch-name)
  (with-slots (file) wad-reader
    (let* ((lump    (get-lump-by-name wad-reader patch-name))
	   (filepos (wad-types::filepos lump))
	   header)
      (file-position file filepos)
      (setf header (read-value 'patch-header file))

      (with-slots (wad-types::filepos wad-types::columnofs wad-types::width) header
	(setf wad-types::filepos filepos)
	(setf wad-types::columnofs
	      (read-value 'binary-element-list file :element-type 'uint32
						    :length wad-types::width)))
      header)))

(defmethod read-texture-header (wad-reader texture-lump-name)
  (with-slots (file) wad-reader
    (let* ((lump (get-lump-by-name wad-reader texture-lump-name))
	   (offset (wad-types::filepos lump))
	   header)
      (file-position file offset)
      (setf header (read-value 'texture-header file))
      (setf (wad-types::offset header) offset)

      (with-slots (wad-types::num-tex wad-types::dataofs) header
	(setf wad-types::dataofs
	      (read-value 'binary-element-list file :element-type 'uint32
						    :length wad-types::num-tex)))
      header)))
  

; ---- LUMP HANDLING ----

(defmethod get-lump-index ((reader wad-reader) lump-name)
  (with-slots (directory numlumps) reader
    (loop for lump in directory
	  for i from 0 upto (1- numlumps)
	  do (if (string-equal (wad-types::name lump) lump-name)
		 (return i)))))

(defmethod get-lump-by-index (wad-reader lump-index)
  (nth lump-index (wad-directory wad-reader)))

(defmethod get-lump-by-name (wad-reader lump-name)
  (get-lump-by-index wad-reader (get-lump-index wad-reader lump-name)))

(defun get-map-lump-index (map-index lump-name)
  (+ map-index (position lump-name *map-lumps* :test #'string=) 1))
  
(defun get-lumps (wad-reader &key start-marker end-marker (include-markers nil))
  (let ((start-id (if start-marker (get-lump-index wad-reader start-marker) 0))
	(end-id   (if end-marker   (get-lump-index wad-reader end-marker)   0))
	(directory (slot-value wad-reader 'directory)))
    (if include-markers
	(setf end-id   (1+ end-id))
	(setf start-id (1+ start-id)))
    (subseq directory start-id end-id)))

(defmethod get-lump-data (wad-reader lump-index &key (header-length 0) (element-type 'uint8) (sizeof-element 1))
  (let* ((lump  (get-lump-by-index wad-reader lump-index))
	 (count (/ (- (wad-types::size lump) header-length) sizeof-element)))
    (file-position (file wad-reader) (wad-types::filepos lump))
    (read-value 'binary-element-list (file wad-reader)
		:element-type element-type :length count)))


; ----- MAP-DATA -----

(defmacro define-get-map-lump (name lump-name element-type sizeof-element)
  `(defmethod ,name (wad-reader map-index)
     (let ((index (get-map-lump-index map-index ,lump-name)))
       (get-lump-data wad-reader index :header-length 0
				       :element-type ,element-type
				       :sizeof-element ,sizeof-element))))

(define-get-map-lump get-things   "THINGS"   'thing     10)
(define-get-map-lump get-linedefs "LINEDEFS" 'linedef   14)  
(define-get-map-lump get-sidedefs "SIDEDEFS" 'sidedef   30)  
(define-get-map-lump get-vertexes "VERTEXES" 'vertex     4)
(define-get-map-lump get-segs     "SEGS"     'seg       12)
(define-get-map-lump get-ssectors "SSECTORS" 'subsector  4)
(define-get-map-lump get-nodes    "NODES"    'node      28)
(define-get-map-lump get-sectors  "SECTORS"  'sector    26)
     

; ----- ASSET-DATA -----

(defmethod get-color-palette (wad-reader index)
  (let* ((file     (slot-value wad-reader 'file))
	 (lump     (get-lump-by-name wad-reader "PLAYPAL"))
	 (position (+ (wad-types::filepos lump) (* index (* 256 3)))))
    (file-position file position)
    (read-value 'binary-element-list file :element-type 'color :length 256)))


(defmethod get-patch-column (wad-reader &optional (offset nil))
  (with-slots (file) wad-reader
    (if offset
	(file-position file offset))
    (let ((column (read-value 'patch-column file)))
      (with-slots (wad-types::top-delta wad-types::num-pixel wad-types::data
		   wad-types::pad-pre wad-types::pad-post) column
	(when (not (eql wad-types::top-delta #xFF))
	  (setf wad-types::num-pixel (read-value 'uint8 file))
	  (setf wad-types::pad-pre   (read-value 'uint8 file))
	  (setf wad-types::data
		(read-value 'binary-element-list file :element-type 'uint8
						      :length wad-types::num-pixel))
	  (setf wad-types::pad-post (read-value 'uint8 file))))
      column)))
	
(defun get-pnames (wad-reader)
 (let* ((lump   (get-lump-by-name wad-reader "PNAMES"))
	(offset (wad-types::filepos lump)))
    (with-slots (file) wad-reader
      (file-position file offset)
      (let ((num (read-value 'uint32 file)))
	(read-value 'binary-element-list file :element-type 'ascii-8 :length num)))))


(defun get-texture-map (wad-reader &optional (offset nil))
  (with-slots (file) wad-reader
    (if offset
	(file-position file offset))
    (let ((tex-map (read-value 'texture-map file)))
      (with-slots (wad-types::patch-maps wad-types::p-count) tex-map
	  (setf wad-types::patch-maps
		(read-value 'binary-element-list file :element-type 'patch-map
						      :length wad-types::p-count)))
      tex-map)))

(defun get-flat-data (wad-reader flat-lump)
  (with-slots (file) wad-reader
    (with-slots (wad-types::size wad-types::filepos) flat-lump
      (file-position file wad-types::filepos)
      (read-value 'binary-element-list file :element-type 'uint8 :length 4096))))
