(in-package :pdf)

(eval-when (:load-toplevel :execute :compile-toplevel) (export 'plot-s :pdf))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; splot Vertical axis
;;;
(defclass repeat-vertical-histo-axis (histo-axis)
  ((repeat-count :accessor repeat-count :initform 1 :initarg :repeat-count
		 :documentation "The number of repeats of the axis.  If REPEAT-COUNT is 1 it behaves
the same as a HISTO-AXIS.")
   (group-separation :accessor group-separation :initform 10 :initarg :group-separation
		     :documentation "The gap between the repeating axis."))
  (:documentation "A version of the HISTO-AXIS class which repeats the same axis multiple times.

This is for plots where the horizontal axis is cut up in multiple pieces and the plot pieces are
stacked vertically.   In that case the vertical axis need to be repeated for each plot piece.

E.g.:  

 | ....                  .........
 |.    ...........      . 
 |                ......
 +-------------------------------

Becomes

 | ....
 |.    ...........
 |                .
 :
 |      ...........
 |     . 
 |..... 
 +-----------------

Here the REPEAT-COUNT is 2 and the GROUP-SEPARATION is the length of ':'.
"))

(defun label-pos-repeat-vertical-axis (axis label-index repeat-index)
  (with-slots (height repeat-count group-separation label-names) axis
    (let* ((group-height (/ (- height (* (- repeat-count 1) group-separation)) repeat-count))
	   (group-start  (- height
			    (* repeat-index group-height)
			    (* repeat-index group-separation)))
	   (label-height (/ group-height (length label-names))))
      (- group-start (* (+ 0.5 label-index) label-height)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; drawing

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
;;;
;;; splot horizontal axis
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; drawing

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
;;;
;;; splot main clas
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper functions, make sure they are moved to something
(defun change-color (color-1 color-2 index)
  ""
  (let ((factor (sqrt (or index 0))))
    (mapcar (lambda (a b) (/ (+ a (* factor b)) (1+ factor))) color-1 color-2)))


(defun intersect-intervals (a-1 a-2 b-1 b-2)
  (list (max a-1 b-1) (min a-2 b-2)))

(defun interval-not-empty-p (a-1 a-2)
  (and a-1 a-2 (> a-2 a-1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; drawing

(defmethod marker-path ((plot plot-s) x y ms)
  "Draw a little triangle pointing left or right.

The triangles vertical side is centered on x y.
the height is 2 |ms| and the point is at ms+x y."
  (move-to x (+ y ms))
  (line-to (+ x ms) y)
  (line-to x (- y ms))
  (pdf:close-path))


(defmethod grid-path ((plot plot-s))
  "Draws the grid for plot."
  (let ((height (height plot)))
    (loop :for tick-x :across (ticks-positions (x-axis plot)) :do
      (move-to tick-x 0)
      (line-to tick-x height))))    


(defmethod draw-object ((plot plot-s))
  (let ((height (height plot))
	(width (width plot))
	(min-value-x (axis-min (x-axis plot)))
	(max-value-x (axis-max (x-axis plot)))
	(mark-start/stop   t)
	(line-width (line-width plot)))
    (with-saved-state
      (translate (x plot) (y plot))

      (set-line-width (line-width (x-axis plot)))
      (grid-path plot)
      (stroke)
      
      (set-line-width line-width)
      (pdf:set-line-cap 0)
      (loop :with repeat-count = (repeat-count plot)
	    :with range-x-block = (/ (- max-value-x min-value-x) repeat-count)
	    :for block :from 0
	    :repeat repeat-count
	    :for min-x = (+ min-value-x (* block range-x-block))
	    :for max-x = (+ min-x range-x-block)
	    :with scale-x =  (* (axis-scale (x-axis plot)) repeat-count)
	    :do
	       (loop :with y-axis = (y-axis plot)
		     :for start-marker = nil
		     :for stop-marker  = nil
		     :for mark-positions = (list)
		     :with ms = (/ (marker-size plot) 2)
		     :for serie :in (series plot)
		     :for serie-count :from 0
		     :for (ln color) :in (labels&colors plot)
		     :for last-color = nil
		     :for y = (label-pos-repeat-vertical-axis y-axis serie-count block)
		     :for grouped-serie = (group-by:group-by serie :key #'third :value #'identity)
		     :do
			(with-saved-state
			  (set-line-width (line-width (y-axis plot)))
			  (set-rgb-stroke 0 0 0)
			  (pdf:set-dash-pattern '(1 10) 0)
			  (move-to 0 y)
			  (line-to width y)
			  (stroke))

			(loop :for (depth . sub-serie) :in (sort grouped-serie (lambda (x y) (< (or x -1) (or y -1))) :key #'car)
			      :do
				 (loop :for (sx1 sx2 depth data-color) :in sub-serie
				       :for (ix1 ix2) = (when (and sx2 sx1) (intersect-intervals min-x max-x sx1 sx2))
				       :for x1 = (when ix1 (* (max 0 (- ix1 min-x)) scale-x))
				       :for x2 = (when ix2 (* (min range-x-block (- ix2 min-x)) scale-x))
				       :for d = (or depth 0)
				       :for adj-y = (+ y (* line-width d))
				       :for col = (change-color (or data-color color) '(1 1 1) depth)
				       :do
					  (unless (equalp last-color col)
					    (stroke)
					    (apply #'set-rgb-stroke col)
					    (apply #'set-rgb-fill col)
					    (setf last-color col))
					  (when (and x1 x2 (interval-not-empty-p x1 x2))
					    (setf start-marker (or start-marker (< sx1 min-x)))
					    (setf stop-marker (or stop-marker (> sx2 max-x)))
					    (when mark-start/stop
					      (when (< min-x ix1 (+ min-x range-x-block)) (push (list x1 adj-y) mark-positions))
					      (when (< min-x ix2 (+ min-x range-x-block)) (push (list x2 adj-y) mark-positions)))
					    (move-to x1 adj-y)
					    (line-to x2 adj-y)))
				 (stroke))
			(when (or start-marker stop-marker mark-positions)
			  (with-saved-state
			    (set-rgb-stroke 0 0 0)
			    (set-rgb-fill 0 0 0)
			    (when start-marker (marker-path plot  0 y (- ms)))
			    (when stop-marker  (marker-path plot  width y ms))
			    (pdf:fill-path)
			    (when mark-positions
			      (set-line-width 0.5)
			      (loop :for (x y) :in (remove-duplicates mark-positions) :do
				(move-to x (- y ms))
				(line-to x (+ y ms)))
			      (pdf:stroke)))))))
    (draw-object (x-axis plot))
    (draw-object (y-axis plot))))





