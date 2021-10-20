;;;; wo-box.asd

(asdf:defsystem #:wo-box
  :description "Describe wo-box here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-pdf #:cl-typesetting #:group-by)
  :components ((:file "package")
               (:file "wo-box")
	       (:file "test-plot")))
