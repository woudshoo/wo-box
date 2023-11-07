(in-package :pdf)

(eval-when (:load-toplevel :execute :compile-toplevel) (export 'mini-plot :pdf))


(defgeneric transform-value (transform value)
  (:method ((axis axis-scale) (value number))
    (with-slots (transform-fn axis-min axis-max axis-scale) axis
      (let ((min (funcall transform-fn axis-min))
	    (max (funcall transform-fn axis-max)))
	(* axis-scale (/ (- (funcall transform-fn value) min) (- max min)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass horizontal-mini-axis (chart-item axis-scale)
  ((color-bands :initarg :color-bands :accessor color-bands :initform nil
		:documentation "Contains a list of triples (lower-bound upper-bound color)")
   (color-serie :initarg :color-serie :accessor color-serie :initform nil)))

(defmethod initialize-instance :after ((axis horizontal-mini-axis) &key v-color-bands &allow-other-keys)
  (setf (axis-scale axis) (width axis))
  (when v-color-bands (setf (color-bands axis) v-color-bands)))

(defmethod draw-object ((axis horizontal-mini-axis))
  (loop :for ((x-1 y-1) (x-2 y-2)) :on (color-serie axis)
	:while (and x-2 y-2)
	:do
	(loop :for (l-y h-y color) :in (color-bands axis)
	      :when (<= l-y y-1 h-y)
		:do
		   (set-color-stroke color)
		   (move-to (transform-value axis x-1) 0)
		   (line-to (transform-value axis x-2) 0)
		   (stroke))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass vertical-mini-axis (chart-item axis-scale)
  ((color-bands :initarg :color-bands :accessor color-bands :initform nil
		:documentation "Contains a list of triples (lower-bound upper-bound color)")))

(defmethod initialize-instance :after ((axis vertical-mini-axis) &key &allow-other-keys)
  (setf (axis-scale axis) (height axis)))

(defmethod draw-object ((axis vertical-mini-axis))
  (loop :for (a b color) :in (color-bands axis)
	:for l-y = (transform-value axis a)
	:for h-y = (transform-value axis b)
	:do
	   (apply #'set-rgb-fill color)
	   (rectangle 0 l-y (width axis) (- h-y l-y))
	   (fill-path)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun serie-min-x (serie)
  (reduce #'min serie :key #'first))

(defun serie-max-x (serie)
  (reduce #'max serie :key #'first))

(defun serie-min-y (serie)
  (reduce #'min serie :key #'second))

(defun serie-max-y (serie)
  (reduce #'max serie :key #'second))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass mini-plot (chart-item)
  ((x-axis :accessor x-axis)
   (y-axis :accessor y-axis)
   (labels&colors :accessor labels&colors :initform nil :initarg :labels&colors)
   (series :accessor series :initform '() :initarg :series)))


(defmethod initialize-instance :after ((plot mini-plot) &rest init-options &key x-axis-min x-axis-max y-axis-min y-axis-max y-transform &allow-other-keys)
  (let ((width (width plot))
	(height (height plot))
	(series (series plot)))
    (setf (x-axis plot)
	  (apply #'make-instance 'horizontal-mini-axis
		 :axis-min (or x-axis-min (reduce #'min  series :key #'serie-min-x))
		 :axis-max (or x-axis-max (reduce #'max  series :key #'serie-max-x))
		 init-options))
    (setf (y-axis plot)
	  (apply #'make-instance 'vertical-mini-axis
		 :transform-fn (or y-transform #'identity)
		 :axis-min (or y-axis-min (reduce #'min series :key #'serie-min-y))
		 :axis-max (or y-axis-max (reduce #'max series :key #'serie-max-y))
		 init-options))))



(defmethod draw-object ((plot mini-plot))
  (let ((width (width plot))
	(height (height plot))
	(x-axis (x-axis plot))
	(y-axis (y-axis plot)))

    (with-saved-state
      (translate (x plot) (y plot))

      (draw-object (y-axis plot))
      (draw-object (x-axis plot))

      (set-line-width (line-width plot)) 
      (set-line-cap 0)

      (loop
	:for serie :in (series plot)
	:for (ln color) :in (labels&colors plot)
	:for (x y) = (first serie)
	:do
	   (set-color-stroke color)
	   (move-to (transform-value x-axis x) (transform-value y-axis y))
	   (loop :for (s-x s-y) :in (rest serie)
		 :for x = (transform-value x-axis s-x)
		 :for y = (transform-value y-axis s-y)
		 :do
	     (line-to x y))
	   (stroke)))))

