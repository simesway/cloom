(require 'uiop)
(require "asdf")
(load "../cloom.asd")
(load "~/quicklisp/setup.lisp")
(asdf:load-system "cloom")

(defparameter *wad-filepath* "../../data/DOOM1.WAD")
(defparameter *data-sequence* nil)

(defun update-and-wait (renderer)
  (view-renderer:update renderer)
  (sleep 0.033333))

(defun set-filepath (path-string)
  (let ((path (uiop:file-exists-p path-string)))
    (if path
	(setf *wad-filepath* path)
	(format t "given filepath '~a' does not exist~%" path-string))))

(defun handle-arguments ()
  (let* ((cmdline (uiop:command-line-arguments))
	 (length  (length cmdline))
	 (path    (first cmdline))
	 (mode    (second cmdline)))
    (if (< length 1)
	(format t "asset-viewer [filepath] [--textures|--sprites|--flats|--all]~%"))
    (set-filepath path)))
	  
      

(defun view-loop (renderer data)
  (let ((updated t)
	(pixel-mode :square-pixel)
	(i 0) (x 0) (y 0))
    (loop :named driver-loop
	  :for c := (charms:get-char (view-renderer::window renderer) :ignore-error t)
	  :do (progn
		(case c
		  (#\q   (return-from driver-loop))
		  (#\Esc (return-from driver-loop))
		  (#\h (progn (setf i (1- i))
			      (setf x 0)
			      (setf y 0)))
		  (#\l (progn (setf i (1+ i))
			      (setf x 0)
			      (setf y 0)))
		  (#\w (setf y (+ y 1)))
		  (#\a (setf x (+ x 2)))
		  (#\s (setf y (- y 1)))
		  (#\d (setf x (- x 2)))
		  (#\y (setf pixel-mode :square-pixel))
		  (#\x (setf pixel-mode :half-pixel))
		  (#\m (if (eql pixel-mode :square-pixel)
			   (setf pixel-mode :half-pixel)
			   (setf pixel-mode :square-pixel)))
		  (-1  nil))
		(if (not (eql c -1))
		    (setf updated t))

		(let* ((index  (mod i (length data)))
		       (object (nth index data))
		       (image  (asset-data::image object)))
		  (if updated
		      (progn
			(charms:clear-window (view-renderer::window renderer))
			(view-renderer::draw-image2 renderer image
						    :pixel-mode pixel-mode :centered t :x x :y y)
			(view-renderer:update renderer)
			(setf updated nil))
		      (sleep 0.01)))))))

(defun main ()
  (handle-arguments)
  (let* ((reader (wad-reader:wad-reader-init *wad-filepath*))
	 (asset-data (asset-data::get-asset-data reader))
	 (renderer (view-renderer:view-renderer-init asset-data))
	 (data (asset-data::sprites asset-data))
	 (images (asset-data::get-images reader)))
    (view-loop renderer data)
    (view-renderer:view-renderer-close)
    (wad-reader:wad-reader-close reader)))

	

(sb-ext:save-lisp-and-die "asset-viewer2" :toplevel #'main :executable t)
