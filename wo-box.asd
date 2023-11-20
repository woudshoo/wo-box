;;;; wo-box.asd

(asdf:defsystem #:wo-box
  :description "Describe wo-box here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-pdf #:cl-typesetting #:group-by #:local-time #+nil #:cl-typegraph)
  :components ((:file "package")
	       (:file "splot")
;	       (:file "mini-plot")
               (:file "wo-box")
	       (:file "flag")
#+nil	       (:file "test-plot")
#+nil	       (:file "version-1")))

