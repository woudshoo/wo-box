(in-package :pdf)

(eval-when (:load-toplevel :execute :compile-toplevel) (export 'plot-flag :pdf))


(defclass plot-flag (chart-item)
  ((series :accessor series :initform (list) :initarg :series)
   (v-scale :accessor v-scale :initform 1.0 :initarg :scale)
   (labels&colors :accessor labels&colors :initform (list) :initarg :labels&colors))
  (:documentation "Make a flag plot, which looks like this:

|    |    |
|---------|
|  |      |
|---------|
|    |    |
|    |    |


where each stripe is indicated by two numbers (a . b).
the number b determines the height of th stripe, and the number a the distance of the middle '|'.

"

   
   ))

(defmethod draw-object ((plot plot-flag))
  (with-saved-state
    (translate (x plot) (y plot))

    (loop
      :with w = (width plot)
      :with s-f = (* (height plot) (v-scale plot))
      :for s-y = (height plot) :then (- s-y s-h)
      :for s :in (series plot)
      :for (label colors) :in (labels&colors plot)
      :for total-s = (reduce #'+ s)
      :for s-h = (* s-f total-s)
      :do
	 (loop :for s-x = 0 :then (+ s-x s-wp)
	       :for s-w :in s
	       :for col :in colors
	       :for s-wp = (* w s-w (/ total-s))
	       :do
		  (rectangle s-x s-y s-wp (- s-h))
		  (set-color-fill col)
		  (set-transparency (or (fourth col) 1.0))
		  (fill-path)))))



