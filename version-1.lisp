(in-package #:wo-box)

(import 'tt::graph)
(import 'tt::graph-node)
(import 'tt::graph-edge)

(defun gen-box-graph ()
  (let* ((g1 (make-instance 'graph :dot-attributes '(#+nil("rankdir" "LR")("nodesep" "0.3")("ranksep" "0.8"))
			    :max-dx 500 :max-dy 300 :border-width nil))
	 (n1 (make-instance 'graph-node :data "box" :graph g1))
	 (n2 (make-instance 'graph-node :data "h-mode-mixin" :graph g1))
	 (n3 (make-instance 'graph-node :data "v-mode-mixin" :graph g1))
	 (n5 (make-instance 'graph-node :data "soft-box" :graph g1))
	 (n6 (make-instance 'graph-node :data "container-box" :graph g1))
	 (n7 (make-instance 'graph-node :data "vbox" :graph g1))
	 (n8 (make-instance 'graph-node :data "hbox" :graph g1))
	 (n9 (make-instance 'graph-node :data "glue" :graph g1))
	 (n10 (make-instance 'graph-node :data "hglue" :graph g1))
	 (n11 (make-instance 'graph-node :data "vglue" :graph g1))
	 (n12 (make-instance 'graph-node :data "spacing" :graph g1))
	 (n13 (make-instance 'graph-node :data "h-spacing" :graph g1))
	 (n14 (make-instance 'graph-node :data "v-spacing" :graph g1))
	 (n15 (make-instance 'graph-node :data "char-box" :graph g1))
	 (n16 (make-instance 'graph-node :data "white-char-box" :graph g1)))
    (tt::add-rank-constraint g1 "same" (list n1 n5 n15))
    (make-instance 'graph-edge :head n1 :tail n5 :label "subclass" :graph g1)
    (make-instance 'graph-edge :head n5 :tail n6 :graph g1)
    (make-instance 'graph-edge :head n6 :tail n7 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n7 :graph g1)
    (make-instance 'graph-edge :head n6 :tail n8 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n8 :graph g1)
    (make-instance 'graph-edge :head n5 :tail n9 :graph g1)
    (make-instance 'graph-edge :head n9 :tail n10 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n10 :graph g1)
    (make-instance 'graph-edge :head n9 :tail n11 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n11 :graph g1)
    (make-instance 'graph-edge :head n5 :tail n12 :graph g1)
    (make-instance 'graph-edge :head n12 :tail n13 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n13 :graph g1)
    (make-instance 'graph-edge :head n12 :tail n14 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n14 :graph g1)
    (make-instance 'graph-edge :head n1 :tail n15 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n15 :graph g1)
    (make-instance 'graph-edge :head n10 :tail n16 :graph g1)
    (tt::compute-graph-layout g1)
    g1))

#+nil (defun create-diagram-pdf (&key in-file out-file)
  (let ((cc  (gen-box-graph)))
    (tt:with-document ()
      (pdf:with-page (:bounds
		      (make-array 4 :initial-contents `(-10 -10 ,(+ 10 (tt::dx cc)) ,(+ (tt::dy cc) 10)))) 
	(tt::stroke cc  0 (+ (tt::dy cc) 0)))
;      (when pdf:*page* (typeset:finalize-page pdf:*page*))
      (pdf:write-document out-file))))




(defun create-diagram-pdf (&key in-file out-file)
  (let ((graph (graph-from-spec-file in-file)))
    (tt::compute-graph-layout graph)
    (tt:with-document ()
      (pdf:with-page (:bounds
		      (make-array 4 :initial-contents `(-10 -10 ,(+ 10 (tt::dx graph)) ,(+ (tt::dy graph) 10)))) 
	(tt::stroke graph  0 (+ (tt::dy graph) 0)))
;      (when pdf:*page* (typeset:finalize-page pdf:*page*))
      (pdf:write-document out-file))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPEC Parsing
(defpackage spec
  (:use common-lisp)
  (:documentation "Package that is used to read SPEC files.

The idea is to use READ to read the spec file and all symbols will
be interned in the SPEC package.  However, not sure if we should use the COMMON-LISP package.
One idea is to allow code to be executed later with EVAL.
"))

(defun parse-graph-package ()
  "Returns the package used to read the SPEC files"
  (find-package 'spec))

(defparameter *current-graph* nil "Current GRAPH to be creaded when converting SPEC to GRAPH")
(defparameter *current-id-node-map* nil "Maps ids to GRAPH nodes")

(defun register-node (id node)
  (setf (gethash id *current-id-node-map*) node))

(defun get-node (id)
  (gethash id *current-id-node-map*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPEC Entry forms


(defun spec::diagram (&rest entries)
  (let ((*current-graph* (make-instance 'graph))
	(*current-id-node-map* (make-hash-table)))
    (dolist (entry entries)
      (spec-to-graph-entry entry))
    *current-graph*))

#+nil (defun spec::node (id label)
  (let ((node (make-instance 'graph-node :data label :graph *current-graph*)))
    (register-node id node)
    node))

(defun spec::node (id label)
  (let* ((text (tt:compile-text (:font "Times-Roman" :font-size 12 :color '(1 0 0)) (tt::put-string label)))
	 (vbox (tt::make-filled-vbox text 70 400))
	 (node (make-instance 'graph-node :data vbox :graph *current-graph*)))
    (register-node id node)
    node))

(defun spec::-> (id-a id-b)
  (make-instance 'graph-edge :graph *current-graph*
			     :head (get-node id-a) :tail (get-node id-b)
			     :edge-arrows  '(:head :arrow)))

(defun spec::-o (id-a id-b)
  (make-instance 'graph-edge :graph *current-graph*
			     :head (get-node id-a) :tail (get-node id-b)
			     :edge-arrows  '(:head :circle)))

(defun spec::<- (id-a id-b)
  (make-instance 'graph-edge :graph *current-graph*
			     :head (get-node id-a) :tail (get-node id-b)
			     :edge-arrows '(:tail :arrow)))

(defun spec::-- (id-a id-b)
  (make-instance 'graph-edge :graph *current-graph*
			     :head (get-node id-a) :tail (get-node id-b)
			     :edge-arrows nil))

(defun spec::<-> (id-a id-b)
  (make-instance 'graph-edge :graph *current-graph*
			     :head (get-node id-a) :tail (get-node id-b)
			     :edge-arrows '(:tail :arrow :head :arrow)))

(defun spec-to-graph-entry (spec)
  "Converts a SPEC into a TT:GRAPH object.

A SPEC is a lisp form of the form

  SPEC := (diagram [NODE | EDGE]*)

Where

 NODE := (node <NODE OPTIONS>)
 EDGE := ([--|<-|->|<->] <EDGE OPTIONS>)

...
"
  (if (fboundp (car spec))
      (apply (car spec) (cdr spec))
      (error "Symbol ~A is not defined" (car spec))))


(defun parse-graph-spec (file-name)
  (with-open-file (s file-name)
    (let ((*package* (parse-graph-package)))
      (let ((spec (read s)))
	spec))))

(defun graph-from-spec-file (file-name)
  "Read a SPEC from file FILE-NAME and returns a GRAPH object"
  (spec-to-graph-entry (parse-graph-spec file-name)))
