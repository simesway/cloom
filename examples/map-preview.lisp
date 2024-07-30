(require 'uiop)
(require "asdf")
(load "../cloom.asd")
(load "~/quicklisp/setup.lisp")
(asdf:load-system 'cloom)


;;;; MAP-PREVIEW: program to view a DOOM map from the terminal
;;;; usage: map-preview [filepath] [map]
;; map is e.g. E1M1


(defparameter *wad-filepath* "../../data/DOOM1.WAD")
(defparameter *map-name* "E1M1")

(defun view-loop (renderer bsp player)
  (let* ((step 5)
	 (rot-speed 5))
    (loop :named driver-loop
	  :for c := (charms:get-char (view-renderer::window renderer) :ignore-error t)
	  :do (progn
		(case c
		  (#\q   (return-from driver-loop))
		  (#\Esc (return-from driver-loop))
		  (#\w (player:move-player player :direction :forward))
		  (#\a (player:move-player player :direction :left))
		  (#\s (player:move-player player :direction :backward))
		  (#\d (player:move-player player :direction :right))
		  (#\l (player:rotate-player player :direction :right))
		  (#\h (player:rotate-player player :direction :left))
		  (-1  nil))
		(charms:clear-window (view-renderer::window renderer))
		(seg-handler::reset-clipping (bsp::seg-handler bsp))
		(bsp::render-bsp bsp)
		(view-renderer::write-str renderer 2 2 (format nil "(~4a,~4a)" (player::x player) (player::y player)))
		(view-renderer:update renderer)
		(sleep 0.001)))))

(defun handle-arguments ()
  (let* ((cmdline (uiop:command-line-arguments))
	 (length  (length cmdline))
	 (path    (first cmdline))
	 (map     (second cmdline)))
    (if (< length 1)
	(format t "map-preview [filepath] [map]~%"))
    (if path
	(setf *wad-filepath* path))
    (if map
	(setf *map-name* map))))

(defun main ()
  (handle-arguments)
  (let* ((map        (map-data::map-data-init *wad-filepath* *map-name*))
	 (player     (player::player-init map))
	 (reader     (wad-reader:wad-reader-init *wad-filepath*))
	 (asset-data (asset-data::get-asset-data reader))
	 (renderer   (view-renderer:view-renderer-init asset-data))
	 (bsp        (bsp::make-bsp map player renderer)))
    ;(charms:clear-window (view-renderer::window renderer))
    ;(bsp::render-bsp bsp)
    ;(view-renderer:update renderer)
    (view-renderer::write-str renderer 2 2 (format nil "(~4a,~4a)" (player::x player) (player::y player)))
    (view-renderer:update renderer)
    ;(sleep 2)
    (view-loop renderer bsp player)
    (view-renderer:view-renderer-close)
    (wad-reader:wad-reader-close reader)))

(sb-ext:save-lisp-and-die "map-preview" :toplevel #'main :executable t)
