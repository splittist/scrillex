;;;; scrillex.asd

(asdf:defsystem #:scrillex
  :description "A scribble syntax for WordprocessingML"
  :author "John Q. Splittist <splittist@splittist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:scribble
	       #:docxplora

	       #:3bmd
	       #:3bmd-ext-tables

	       #:alexandria
	       #:serapeum
	       #:split-sequence
	       #:plump)
  :components ((:file "package")
	       (:file "style")
	       (:file "mdprinter")
	       (:file "scrillex")))
