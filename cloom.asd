(defsystem "cloom"
  :description "common lisp command line doom"
  :version "1.0"
  :author "simon"
  :serial t
  :components ((:file "src/binary-reader")
	       (:file "src/wad-types")
	       (:file "src/wad-reader")
	       (:file "src/asset-data")
	       (:file "src/map-data")
	       (:file "src/view-renderer")))
