(defpackage :bsp
  (:use :common-lisp :utilities)
  (:export :get-player-height :stop-traversing-bsp))

(in-package :bsp)


(defclass bsp ()
  ((seg-handler :accessor seg-handler)
   (nodes :accessor nodes)
   (ssectors :accessor ssectors)
   (segs     :accessor segs)
   (player   :accessor player)
   (root-node-id :accessor root-node-id)
   (is-traverse  :accessor is-traverse
		 :initform t)))

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

(defun get-player-height (bsp)
  (+ player:HEIGHT (get-subsector-height bsp)))

(defun stop-traversing-bsp (bsp)
  (setf (is-traverse bsp) nil))
