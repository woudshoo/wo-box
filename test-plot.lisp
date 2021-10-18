(in-package #:wo-box)

(eval-when (:compile-toplevel :load-toplevel :execute)
(import 'cl-pdf::chart-item)
(import 'cl-pdf::x)
(import 'cl-pdf::y)
(import 'cl-pdf::height)
(import 'cl-pdf::width)
(import 'cl-pdf::vertical-value-axis)
(import 'cl-pdf::horizontal-value-axis)
(import 'cl-pdf::draw-object)
(import 'cl-pdf::ticks-positions)
(import 'cl-pdf::tick-width)
(import 'cl-pdf::tick-length)
(import 'cl-pdf::vertical-histo-axis)
(import 'cl-pdf::histo-axis)
(import 'cl-pdf::label-color)
(import 'cl-pdf::line-width)
(import 'cl-pdf::line-color)
(import 'cl-pdf::label-font-size)
(import 'cl-pdf::label-font)
(import 'cl-pdf::label-names)
(import 'cl-pdf::draw-left-text)
(import 'cl-pdf::line-width)
(import 'cl-pdf::background-color)
(import 'cl-pdf::labels&colors)
(import 'cl-pdf::axis)
(import 'cl-pdf::axis-min)
(import 'cl-pdf::axis-max)
(import 'cl-pdf::axis-scale)
(import 'cl-pdf::axis-size)
(import 'cl-pdf::title-font-size)

(import 'cl-pdf:translate)
(import 'cl-pdf:with-saved-state)
(import 'cl-pdf:set-rgb-fill)
(import 'cl-pdf:set-rgb-stroke)
(import 'cl-pdf:basic-rect)
(import 'cl-pdf:set-line-width)

(import 'cl-pdf:move-to)
(import 'cl-pdf:line-to)
(import 'cl-pdf:stroke)
(import 'cl-pdf:fill-and-stroke))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util
(defun timestamp-iso-week (timestamp)
  (multiple-value-bind (year week-nr day-of-week)
      (local-time::%timestamp-decode-iso-week timestamp)
    week-nr))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SPLOT vertical axis
;;
(defclass repeat-vertical-histo-axis (histo-axis)
  ((repeat-count :accessor repeat-count :initform 1 :initarg :repeat-count)
   (group-separation :accessor group-separation :initform 10 :initarg :group-separation)))

(defun label-pos-repeat-vertical-axis (axis label-index repeat-index)
  (with-slots (height repeat-count group-separation label-names) axis
    (let* ((group-height (/ (- height (* (- repeat-count 1) group-separation)) repeat-count))
	   (group-start  (- height
			    (* repeat-index group-height)
			    (* repeat-index group-separation)))
	   (label-height (/ group-height (length label-names))))
      (- group-start (* (+ 0.5 label-index) label-height)))))

(defmethod draw-object ((axis repeat-vertical-histo-axis))
  (with-saved-state
    (translate (x axis) (y axis))
    (set-line-width (line-width axis))
    (apply #'set-rgb-stroke (line-color axis))
    (move-to 0 0)
    (line-to 0 (height axis))
    (stroke)

    (apply #'set-rgb-fill (label-color axis))
    (loop :for repeat-index :from 0
	  :repeat (repeat-count axis)
	  :do
	     (loop :for label :in (label-names axis)
		   :for label-count :from 0
		   :with font = (label-font axis)
		   :with font-size = (label-font-size axis)
		   :with font-metrics = (pdf:font-metrics font)
		   :for y = (-  (label-pos-repeat-vertical-axis axis label-count repeat-index) (* 0.5 font-size (pdf:cap-height font-metrics)))
		   :with x = (* -1.25 font-size)
		   :with max-width = (or (width axis) 50)
		   :do
		      (draw-left-text x y label font font-size max-width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPLOT horizontal axis
;;;
(defclass horizontal-splot-axis (axis)
  ((min-value :accessor min-value :initform 0 :initarg :min-value)
   (max-value :accessor max-value :initform 100 :initarg :max-value)
   (repeat-count :accessor repeat-count :initform 1 :initarg :repeat-count)
   (ticks-positions :accessor ticks-positions :initform #())
   (axis-scale :accessor axis-scale)
   (nb-ticks :accessor nb-ticks :initform 8 :initarg :nb-ticks)
   (axis-min :accessor axis-min)
   (axis-max :accessor axis-max)))


(defmethod initialize-instance :after ((axis horizontal-splot-axis) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (with-slots (min-value max-value axis-min axis-max axis-scale ticks-positions) axis
    (setf axis-min (floor min-value))
    (setf axis-max (ceiling max-value))
    (setf axis-scale (/ (axis-size axis) (- axis-max axis-min)))
    (setf ticks-positions (coerce  (loop :for i :from 0
					 :repeat (nb-ticks axis)
					 :with width = (/ (axis-size axis) (1- (nb-ticks axis)))
					 :collect (* i width))
				   'vector))))

(defmethod draw-object ((axis horizontal-splot-axis))
  (with-saved-state
    (translate (x axis) (y axis))
    (set-line-width (line-width axis))
    (apply #'set-rgb-stroke (line-color axis))
    (move-to 0 0)
    (line-to (width axis) 0)
    (stroke)
    (set-line-width (tick-width axis))
    (loop :for tick-pos :across (ticks-positions axis)
	  :with tick-depth = (- (tick-length axis))
	  :do
	     (move-to tick-pos 0)
	     (line-to tick-pos tick-depth))
    (stroke)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vertical histo axis draw code
;;;
(defmethod draw-object ((axis vertical-histo-axis))
  (with-saved-state
    (translate (x axis) (y axis))
    (set-line-width (line-width axis))
    (apply #'set-rgb-stroke (line-color axis))
    (move-to 0 0)
    (line-to 0 (height axis))
    (stroke)

    (set-line-width (tick-width axis))
    (move-to 0 0)
    (line-to (- (tick-length axis)) 0)
    (stroke)

    (apply #'set-rgb-fill (label-color axis))
    (loop :with nb = (length (label-names axis))
	  :with d = (/ (height axis) nb)
	  :with l = (- (tick-length axis))
	  :with font-size = (label-font-size axis)
	  :with max-width = (or (width axis) (- d (* 0.6 font-size)))
	  :with text-x = (* -1.25 font-size)
	  :with font = (label-font axis)
	  :with font-metrics = (pdf:font-metrics font)
	  :for name :in (label-names axis)
	  :for ty :from d :by d
	  :for text-y :from (* 0.5 (- d (* font-size (pdf:cap-height font-metrics)))) :by d :do
	    (move-to 0 ty)
	    (line-to l ty)
	    (stroke)
	    (draw-left-text text-x text-y  name (label-font axis) font-size max-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions


(defun intersect-intervals (a-1 a-2 b-1 b-2)
  (list (max a-1 b-1) (min a-2 b-2)))

(defun interval-not-empty-p (a-1 a-2)
  (and a-1 a-2 (> a-2 a-1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPLOT main class
;;;

(defclass plot-s (chart-item)
  ((series :accessor series :initform () :initarg :series)
   (labels&colors :accessor labels&colors :initform (list) :initarg :labels&colors)
   (x-axis :accessor x-axis)
   (y-axis :accessor y-axis)
   (marker-size :accessor marker-size :initform 8.0 :initarg :marker-size)
   (repeat-count :accessor repeat-count :initform 1 :initarg :repeat-count)))


(defmethod initialize-instance :after ((plot plot-s) &rest init-options &key &allow-other-keys)

  (setf (x-axis plot) 	(make-instance 'horizontal-splot-axis
				       :x (x plot)
				       :y (y plot)
				       :width (width plot)
				       :min-value (or (getf init-options :min-value) 0)
				       :max-value (or (getf init-options :max-value) 12.0)))
  (unless (getf init-options :repeat-count)
    (setf (repeat-count plot) (- (axis-max (x-axis plot))
				 (axis-min (x-axis plot)))))
  (unless (getf init-options :height)
    (setf (height plot)
	  (* 1.1 (title-font-size plot) (* (repeat-count plot) (length (labels&colors plot))))))
  (setf (y-axis plot)
	(make-instance 'repeat-vertical-histo-axis
		       :x (x plot)
		       :y (y plot)
		       :height (height plot)
		       :width (or (getf init-options :label-width) 100)
		       :repeat-count (repeat-count plot)
		       :label-names (mapcar #'first (labels&colors plot)))))


(defun draw-marker (x y ms)
  (move-to x (+ y ms))
  (line-to (+ x ms) y)
  (line-to x (- y ms))
  (pdf:close-path))

(defmethod draw-object ((plot plot-s))
  (let ((height (height plot))
	(width (width plot))
	(min-value-x (axis-min (x-axis plot)))
	(max-value-x (axis-max (x-axis plot))))
    (with-saved-state
      (translate (x plot) (y plot))
      (set-line-width (line-width (x-axis plot)))
      (loop :for tick-x :across (ticks-positions (x-axis plot)) :do
	(move-to tick-x 0)
	(line-to tick-x height)
	(stroke))

      (set-line-width (line-width plot))
      (pdf:set-line-cap 0)
      (loop :with repeat-count = (repeat-count plot)
	    :with range-x-block = (/ (- max-value-x min-value-x) repeat-count)
	    :for block :from 0
	    :repeat repeat-count
	    :for min-x = (+ min-value-x (* block range-x-block))
	    :for max-x = (+ min-x range-x-block)
	    :with scale-x =  (* (axis-scale (x-axis plot)) repeat-count)
	    :do
;	       (format t "MIN-X: ~A MAX-X: ~A BLOCK: ~A~%" min-x max-x block)
	       (loop :with y-axis = (y-axis plot)
		     :for start-marker = nil
		     :for stop-marker  = nil
		     :with ms = (/ (marker-size plot) 2)
		     :for serie :in (series plot)
		     :for serie-count :from 0
		     :for (ln color) :in (labels&colors plot)
		     :for y = (label-pos-repeat-vertical-axis y-axis serie-count block)
		     :do
			(apply #'set-rgb-stroke color)
			(apply #'set-rgb-fill color)
			(loop :for (sx1 sx2) :in serie
			      :for (ix1 ix2) = (when (and sx2 sx1) (intersect-intervals min-x max-x sx1 sx2))
			      :for x1 = (when ix1 (* (max 0 (- ix1 min-x)) scale-x))
			      :for x2 = (when ix2 (* (min range-x-block (- ix2 min-x)) scale-x))
			      :do
;				 (format t "   <L ~A: ~A -- ~A, scale ~A ~%" ln x1 x2 scale-x)
				 (when (and x1 x2 (interval-not-empty-p x1 x2))
				   (setf start-marker (or start-marker (< sx1 min-x)))
				   (setf stop-marker (or stop-marker (> sx2 max-x)))
				   (move-to x1 y)
				   (line-to x2 y)))
			(stroke)
			(when (or start-marker stop-marker)
			  (with-saved-state
			    (set-rgb-stroke 0 0 0)
			    (set-rgb-fill 0 0 0)
			    (when start-marker (draw-marker 0 y (- ms)))
			    (when stop-marker  (draw-marker width y ms))
			    (pdf:fill-path))))))
    (draw-object (x-axis plot))
    (draw-object (y-axis plot))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test-plot-s (&optional (file-name #P "/tmp/splot.pdf"))
  (pdf:with-document ()
    (pdf:with-page (:bounds #(0 0 650 400))
      (draw-object
       (make-instance 'plot-s :x 50 :y 50 :width 500
;			      :height 300
			      :line-width 4.0
			      :repeat-count 20
			      :min-value -3
			      :max-value 14
			      :labels&colors '(("a" (1.0 0.0 0.0))
					       #+nil ("B" (0.0 1.0 0.0))
					       #+nil("C" (0.0 0.0 1.0))
					       #+nil ("DD" (1.0 0.0 1.0)))
			      :series '(((0 1) (1.4 3) (4 6))
					((-0.8 2) (2.5 10))
					((-3 -1) (0.2 1.8) (5 7) (8 14))
					((-10 40))))))
    (pdf:write-document file-name)))
