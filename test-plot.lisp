(in-package #:wo-box)

#+nil(eval-when (:compile-toplevel :load-toplevel :execute)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPLOT horizontal axis
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vertical histo axis draw code
;;;
;; (defmethod draw-object ((axis vertical-histo-axis))
;;   (with-saved-state
;;     (translate (x axis) (y axis))
;;     (set-line-width (line-width axis))
;;     (apply #'set-rgb-stroke (line-color axis))
;;     (move-to 0 0)
;;     (line-to 0 (height axis))
;;     (stroke)

;;     (set-line-width (tick-width axis))
;;     (move-to 0 0)
;;     (line-to (- (tick-length axis)) 0)
;;     (stroke)

;;     (apply #'set-rgb-fill (label-color axis))
;;     (loop :with nb = (length (label-names axis))
;; 	  :with d = (/ (height axis) nb)
;; 	  :with l = (- (tick-length axis))
;; 	  :with font-size = (label-font-size axis)
;; 	  :with max-width = (or (width axis) (- d (* 0.6 font-size)))
;; 	  :with text-x = (* -1.25 font-size)
;; 	  :with font = (label-font axis)
;; 	  :with font-metrics = (pdf:font-metrics font)
;; 	  :for name :in (label-names axis)
;; 	  :for ty :from d :by d
;; 	  :for text-y :from (* 0.5 (- d (* font-size (pdf:cap-height font-metrics)))) :by d :do
;; 	    (move-to 0 ty)
;; 	    (line-to l ty)
;; 	    (stroke)
;; 	    (draw-left-text text-x text-y  name (label-font axis) font-size max-width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPLOT main class
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test-plot-s (&optional (file-name #P "/tmp/splot.pdf"))
  (pdf:with-document ()
    (pdf:with-page (:bounds #(0 0 650 800))
      (pdf:draw-object
       (make-instance 'cl-pdf:plot-s :x 50 :y 50 :width 500
;			      :height 300
			      :line-width 4.0
			      :repeat-count 20
			      :min-value -3
			      :max-value 14
			      :labels&colors '(("a" (1.0 0.0 0.0))
					       ("B" (0.0 1.0 0.0))
					       ("C" (0.0 0.0 1.0))
					       ("DD" (1.0 0.0 1.0)))
			      :series '(((0 1) (1.4 3) (4 6))
					((-0.8 2) (2.5 10))
					((-3 -1) (0.2 1.8) (5 7) (8 14))
					((-10 40))))))
    (pdf:write-document file-name)))
