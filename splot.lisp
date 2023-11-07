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


(defmethod group-pos-start ((axis repeat-vertical-histo-axis) (group number))
  "Returns the position on the AXIS for group GROUP.

For y axis, it will return the y positions, for x axis the x position.
GROUP is indicated by an integer, starting with 0."
  (with-slots (height repeat-count group-separation) axis
    (let ((group-height (/ (- height (* (- repeat-count 1) group-separation)) repeat-count)))
      (- height
	 (* group group-height)
	 (* group group-separation)))))

(defmethod group-pos-end ((axis repeat-vertical-histo-axis) (group number))
    "Returns the position on the AXIS for group GROUP.

For y axis, it will return the y positions, for x axis the x position.
GROUP is indicated by an integer, starting with 0."
  (with-slots (height repeat-count group-separation) axis
    (let ((group-height (/ (- height (* (- repeat-count 1) group-separation)) repeat-count)))
      (- height
	 (* group group-height)
	 (* group group-separation)
	 group-height))))


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

    ;; this drew the vertical line next to the labels.
    ;; this line (now partial) is also drawn by code for the grid-path
    ;; in the main plot routine.   So this commenting out is conceptually wrong.
    #+nil     (move-to 0 0)
    #+nil    (line-to 0 (height axis))
    #+nil    (stroke)

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
   (axis-max :accessor axis-max)
   (label-one-tick :accessor label-one-tick :initform nil :initarg :label-one-tick)
   (label-all-ticks :accessor label-all-ticks :initform nil :initarg :label-all-ticks)))


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
    (stroke)
    (alexandria:when-let (label (label-one-tick axis))
      (draw-duration-indicator axis 0 1 (* -1 (tick-length axis)) label))
    (alexandria:when-let (label (label-all-ticks axis))
      (draw-duration-indicator axis 0 (-  (nb-ticks axis) 1) (* -2.5 (tick-length axis)) label))))


(defun draw-arrow-head (x1 y1 x2 y2 &key arrow-length arrow-width)
  "Draw a arrow head at x2 y2.   The arrow is pointing away from the line (x1 y1)-(x2 y2)."
    (let* ((nx (- x1 x2))
	   (ny (- y1 y2))
	   (l (/ (sqrt (+ (* nx nx) (* ny ny)))))
	   (x0 (+ x2 (* nx arrow-length l)))
	   (y0 (+ y2 (* ny arrow-length l)))
	   (dx (* nx arrow-width l))
	   (dy (* ny arrow-width l)))
      (pdf:move-to x2 y2)
      (pdf:line-to (+ x0 dy) (- y0 dx))
      (pdf:line-to (- x0 dy) (+ y0 dx))
      (pdf:line-to x2 y2)
      (pdf:fill-and-stroke)))

(defun draw-line-with-arrow (x1 y1 x2 y2 &key (arrow-length 3) (arrow-width 2))
  "Draw a line ending in an arrow.  Line is from (x1 y1) to (x2 y2), arrows point is at (x2 y2)."
  (pdf:move-to x1 y1)
  (pdf:line-to x2 y2)
  (draw-arrow-head x1 y1 x2 y2 :arrow-length arrow-length :arrow-width arrow-width))


(defmethod draw-duration-indicator ((axis horizontal-splot-axis)
				    (from-tick integer)
				    (to-tick integer)
				    (offset number)
				    (text string))
  "Draw a marker from FROM-TICK to TO-TICK.
The marker will look like this:

   <-- TEXT             -->

where the <-- and --> match the FROM-TICK and TO-TICK.
The OFFSET is the vertical position where it is drawn.

However we might want to interpret OFFSET as relative to the font size?"

  ;; initial no arrow, I need to look up the PDF reference for arrows.

  (set-line-width (line-width axis))
  (apply #'set-rgb-stroke (label-color axis))
  (apply #'set-rgb-fill (label-color axis))
  (let* ((ticks (ticks-positions axis))
	 (from (aref ticks from-tick))
	 (to (aref ticks to-tick))

	 (font (label-font axis))
	 (font-size (label-font-size axis))
	 (font-metrics (pdf:font-metrics font))
	 (font-y-offset (* 0.5 font-size (pdf:cap-height font-metrics)))
	 (font-x-offset (* 0.25 font-size))
	 (marker-width (* 1 (tick-length axis))))

    (draw-line-with-arrow (+ from marker-width) offset from offset)
    (draw-line-with-arrow (- to marker-width) offset to offset)
    ;; need to adjust for font height etc.
    (draw-right-text (+ from marker-width font-x-offset) (- offset font-y-offset) text font font-size)))
  

  
  
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
				       :max-value (or (getf init-options :max-value) 12.0)
				       :nb-ticks (+ 1 (or (getf init-options :nb-x-breaks) 7))
				       :label-font-size 8.0
				       :label-color '(1.0 0.4 0.0)
				       :label-one-tick (getf init-options :label-one-tick)
				       :label-all-ticks (getf init-options :label-all-ticks)))
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
  "Draws the grid for plot. (The vertical lines breaking up the strips.)"
  (let ((y-axis (y-axis plot)))
    (loop :for tick-x :across (ticks-positions (x-axis plot)) :do
      (loop :for group :from 0 :below (repeat-count y-axis) :do
	(move-to tick-x (group-pos-start y-axis group ))
	(line-to tick-x (group-pos-end y-axis group))))))    


(defmethod draw-object ((plot plot-s))
  (let ((width (width plot))
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

      ;;; Loop over the strips/repeats of the blocks
      (loop :with repeat-count = (repeat-count plot)
	    :with range-x-block = (/ (- max-value-x min-value-x) repeat-count)
	    :for block :from 0
	    :repeat repeat-count
	    :for min-x = (+ min-value-x (* block range-x-block))
	    :for max-x = (+ min-x range-x-block)
	    :with scale-x =  (* (axis-scale (x-axis plot)) repeat-count)
	    :do
	       ;;; Loop over the series
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
			;; For each lane in the block first plot the dashed background line
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
					    ;; to optimize number of strokes, only
					    ;; stroke when color changes.
					    (stroke)
					    (apply #'set-rgb-stroke col)
					    (apply #'set-rgb-fill col)
					    (setf last-color col))
					  (cond
					    ((and x1 x2 (interval-not-empty-p x1 x2))
					     ;; normal case
					     ;; This to mark the edge of the swimming lane with continuation triangles
					     (setf start-marker (or start-marker (< sx1 min-x)))
					     (setf stop-marker (or stop-marker (> sx2 max-x)))
					     ;; This sit to mark with bars the beginning and stop of a time range
					     (when mark-start/stop
					       (when (< min-x ix1 (+ min-x range-x-block)) (push (list x1 adj-y) mark-positions))
					       (when (< min-x ix2 (+ min-x range-x-block)) (push (list x2 adj-y) mark-positions)))
					     ;; Here we draw time range
					     (move-to x1 adj-y)
					     (line-to x2 adj-y))

					    ((and (not (interval-not-empty-p sx1 sx2)) (or x1 x2))
					     ;; empty interval, just put a star.
					     (pdf:star (or x1 x2) adj-y ms (/ ms 2) 4))
					    ))

				 (stroke))
			(when (or start-marker stop-marker mark-positions)
			  ;; Put additional markes on the line segments/stars
			  (with-saved-state
			    (set-rgb-stroke 0 0 0)
			    (set-rgb-fill 0 0 0)
			    ;; Add the Continuation triangles at the beginning
			    ;; or end of the swimming lanes
			    (when start-marker (marker-path plot  0 y (- ms)))
			    (when stop-marker  (marker-path plot  width y ms))
			    (pdf:fill-path)
			    ;; Mark the beginning and end of each colored segments with
			    ;; black ticks if requested
			    (when mark-positions
			      (set-line-width 0.5)
			      (loop :for (x y) :in (remove-duplicates mark-positions) :do
				(move-to x (- y ms))
				(line-to x (- y (/ ms 4)))
				(move-to x (+ y (/ ms 4)))
				(line-to x (+ y ms)))
			      (pdf:stroke)))))))
    (draw-object (x-axis plot))
    (draw-object (y-axis plot))))





