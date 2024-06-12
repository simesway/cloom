(asdf:defsystem "cloom"
  :description "common lisp command line doom"
  :version "1.0"
  :author "simon"
  :serial t
  :depends-on ("cl-charms" "trivial-gamekit")
  :components ((:file "src/utilities")
	       (:file "src/binary-reader")
	       (:file "src/wad-types")
	       (:file "src/wad-reader")
	       (:file "src/asset-data")
	       (:file "src/map-data")
	       (:file "src/math")
	       (:file "src/player")
	       (:file "src/view-renderer")
	       (:file "src/seg-handler")
	       (:file "src/bsp")
	       (:file "src/map-renderer")))
