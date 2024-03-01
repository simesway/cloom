(defpackage :asset-data
  (:use :common-lisp :wad-reader)
  (:export :asset-data :patch :texture
           :get-patch :get-sprites :get-textures))
   

(in-package :asset-data)


; ----- PATCHES -----

(defclass patch ()
  ((header  :initarg  :header)
   (name    :accessor name
	    :initarg  :name)
   (width   :accessor width)
   (height  :accessor height)
   (image   :accessor image)
   (columns :initform '()
	    :accessor columns)))

(defmethod get-patch-columns (patch reader)
  (with-slots (header width columns) patch
    (let ((column-offsets (wad-types::columnofs header))
	  (offset         (wad-types::filepos   header)))

      (dotimes (i width)
	(file-position (wad-reader::file reader) (+ offset (nth i column-offsets)))
	(loop
	  (let* ((column    (get-patch-column reader ))
		 (top-delta (wad-types::top-delta column)))
	    (push column columns)
	    (when (eql top-delta #xFF)
	      (return))))))

    (setf columns (nreverse columns))))

(defun get-image (patch)
  (with-slots (width height columns) patch
    (let ((image (make-array (list width height) :initial-element nil))
	  (x 0) (y 0))
      (dolist (column columns)
	(let ((top-delta (wad-types::top-delta column))
	      (color-ids (wad-types::data      column))
	      (length    (wad-types::num-pixel column)))
	  (if (eql top-delta #xFF)
	      (setf x (1+ x))
	      (loop for yi from 0 below length
		    do (let ((color-id (nth yi color-ids))
			     (yd (+ y yi top-delta)))
			 (setf (aref image x yd) color-id))))))
      image)))

(defmethod get-patch (reader patch-name)
  (let* ((header (wad-reader:read-patch-header reader patch-name))
	 (patch  (make-instance 'patch :header header
				       :name   patch-name)))
    (with-slots (width height) patch
      (setf width  (wad-types::width  header))
      (setf height (wad-types::height header)))
    (get-patch-columns patch reader)
    (setf (image patch) (get-image patch))
    patch))


; ----- SPRITES -----

(defun get-sprites (reader)
  (let ((sprite-lumps (get-lumps reader :start-marker "S_START"
					:end-marker   "S_END"))
	(sprites     '()))
    (dolist (lump sprite-lumps)
      (let ((patch-name (wad-types::name lump)))
	(setf sprites (cons (get-patch reader patch-name) sprites))))
    (setf sprites (nreverse sprites))))


; ----- TEXTURES -----

(defclass texture ()
  ((name   :accessor name)
   (width  :accessor width)
   (height :accessor height)
   (image  :accessor image)
   (texmap :accessor texmap
	   :initarg  :texture-map)))

(defun get-texture-patches (reader)
  (let ((pnames (get-pnames reader))
	(texture-patches '()))
    (dolist (pname pnames)
      (push (get-patch reader pname) texture-patches))
    (nreverse texture-patches)))

(defun get-texture-maps (reader texture-lump-name)
  (let* ((header (read-texture-header reader texture-lump-name))
	 (offset  (wad-types::offset  header))
	 (offsets (wad-types::dataofs header))
	 (tex-maps '()))
    (dolist (ofs offsets)
      (setf tex-maps (cons (get-texture-map reader (+ offset ofs)) tex-maps)))
    (setf tex-maps (nreverse tex-maps))))


(defun add-image (destination source &optional x y)
  (let* ((dx (if x x 0))
         (dy (if y y 0))
	 (dims-dest (array-dimensions destination))
	 (dims-src  (array-dimensions source))
	 (src-w      (nth 0 dims-src))
	 (src-h      (nth 1 dims-src))
	 (dest-w     (nth 0 dims-dest))
	 (dest-h     (nth 1 dims-dest)))
    (loop for i from (max 0 (- dx)) below (min src-w (- dest-w dx))
          do (loop for j from (max 0 (- dy)) below (min src-h (- dest-h dy))
                   do (let ((color (aref source i j)))
			(if color
			    (setf (aref destination (+ i dx) (+ j dy)) color)))))
    destination))

(defmethod get-texture-img (texture texture-patches)
  (with-slots (width height texmap) texture
    (let* ((dimensions  (list width height))
	   (texture-img (make-array dimensions :initial-element nil))
	   (patch-maps  (wad-types::patch-maps texmap)))
      (dolist (patch-map patch-maps)
	(with-slots (wad-types::patch wad-types::x-ofs wad-types::y-ofs) patch-map
	  (let* ((patch-id wad-types::patch)
		 (patch    (nth patch-id texture-patches))
		 (x wad-types::x-ofs)
		 (y wad-types::y-ofs))
	    (setf texture-img (add-image texture-img (image patch) x y)))))
      texture-img)))
    

(defun get-texture (texture-map texture-patches)
  (let* ((texture (make-instance 'texture :texture-map texture-map)))
    (with-slots (name width height image) texture
      (setf name   (wad-types::name   texture-map))
      (setf width  (wad-types::width  texture-map))
      (setf height (wad-types::height texture-map))
      (setf image  (get-texture-img texture texture-patches)))
    texture))
  
(defun get-textures (reader texture-lump-name)
  (let* ((texture-patches (get-texture-patches reader))
	 (texture-maps    (get-texture-maps reader texture-lump-name))
	 (textures '()))
    (dolist (map texture-maps)
      (setf textures (cons (get-texture map texture-patches) textures)))
    (nreverse textures)))


; ----- FLATS -----

(defclass flat ()
  ((name  :accessor name)
   (data  :accessor data)
   (image :accessor image)))

(defun get-flats (reader start-marker end-marker)
  (let ((flat-lumps (get-lumps reader :start-marker start-marker
			              :end-marker   end-marker :include-markers t))
	(flats '()))
    (dolist (lump flat-lumps)
      (if (eql (wad-types::size lump) 4096)
	  (setf flats (cons (get-flat reader lump) flats))))
    (nreverse flats)))

(defun get-flat-image (flat)
  (let* ((dimensions (list 64 64))
	 (image (make-array dimensions :initial-element nil))
	 (data  (data flat)))
    (dotimes (i (length data))
      (let ((x     (mod i 64))
	    (y   (floor i 64))
	    (color (nth i data)))
	(if color
	    (setf (aref image x y) color))))
    image))

(defun get-flat (reader flat-lump)
  (let ((flat (make-instance 'flat)))
    (with-slots (name data image) flat
      (setf name  (wad-types::name flat-lump))
      (setf data  (get-flat-data reader flat-lump)))
    (setf (image flat) (get-flat-image flat))
    flat))


; ----- ASSET-DATA -----

(defclass asset-data ()
  ((sprites  :accessor sprites)
   (textures :accessor textures)
   (flats    :accessor flats)))

(defun get-asset-data (reader)
  (let ((data (make-instance 'asset-data))
	(tex1 (get-textures reader "TEXTURE1"))
	(tex2 (get-textures reader "TEXTURE2")))
    (with-slots (sprites textures flats) data
      (setf sprites (get-sprites reader))
      (setf textures (append tex1 tex2))
      (setf flats (get-flats reader "F_START" "F_END")))
    data))
