;;;; wo-box.lisp

(in-package #:wo-box)

;; /usr/local/texlive/2017/texmf-dist/fonts/type1/urw/palatino/uplr8a.pfb
;; /usr/local/texlive/2017/texmf-dist/fonts/afm/urw/palatino/uplr8a.afm

#-win32
(defparameter *default-font-afm* #P"/usr/local/texlive/2017/texmf-dist/fonts/afm/urw/palatino/uplr8a.afm")
#+win32
(defparameter *default-font-afm* #P"c:/context/im/tex/texmf/fonts/afm/public/lm/lmr10.afm")
#-win32
(defparameter *default-font-pfb*  #P "/usr/local/texlive/2017/texmf-dist/fonts/type1/urw/palatino/uplr8a.pfb")
#+win32
(defparameter *default-font-pfb*  #P "c:/context/im/tex/texmf/fonts/type1/public/lm/lmr10.pfb")

#-win32
(defparameter *default-font-name* "URWPalladioL-Roma")
#+win32
(defparameter *default-font-name* "LMRoman10-Regular")


(defclass boxed-content (cl-typesetting:vbox)
  ())

(defmethod cl-typesetting::stroke ((box boxed-content) x y)
  (draw-box x y (cl-typesetting::dx box) (cl-typesetting::dy box))
  (call-next-method box x y))
  
  

(defun kerned-text (text &optional (font pdf::*font*))
  (loop
    :with result = (list)
    :with partial-string = (list)
    :for prev-c = nil :then c
    :for c :across text :do
      (if (or (not prev-c)
	      (= 0 (pdf:get-kerning prev-c c font)))
	  (push c partial-string)
	  (progn
	    (push (coerce (nreverse partial-string) 'string) result)
	    (setf partial-string (list c))
	    (push (* 1000 (- (pdf:get-kerning prev-c c font))) result)))
    :finally (return (nreverse (push (coerce (nreverse partial-string) 'string) result)))))


(defun draw-box (x y dx dy &optional (color "red"))
  (pdf:with-saved-state 
    (pdf:set-color-stroke color)
;    (pdf:set-color-fill "blue")
    (pdf:set-transparency 0.5)
    (pdf:rectangle x y dx (- dy) :radius 5.0)
    (pdf:stroke)))

(defun boxed-text (text &key (width 100) (font-name "Helvetica" #+nil *default-font-name*))
  (let* ((compiled-text (typeset:compile-text (:font font-name)
			  (typeset:paragraph (:h-align :center :font-size 10.0) "Hallo") text)))
    (multiple-value-bind (lines left) (typeset::fit-lines compiled-text width 10000)
      (let ((box (make-instance 'typeset:vbox :boxes lines)))
	(typeset::do-layout box)
	(pdf:move-to 0 0)
	(draw-box 0 0 width (- 10000 left) "green")
#+nil	(draw-box 0 0 width (- 10000 left) "green")
	(draw-box 0 0 (typeset::dx  box) (typeset::dy box))
	(format t "dx: ~A, dy: ~A, left: ~A~%" (typeset::dx box) (typeset::dy box) left)
	(typeset::stroke box 0 0)))))

(defun test (&optional (file-name #P"/tmp/test.pdf"))
  (pdf:with-document ()
#+nil    (pdf:load-t1-font *default-font-afm* *default-font-pfb*)
    (pdf:with-page ()
      (let (#+nil (font-1 (pdf:get-font "Helvetica"))
	    #+nil (font-2 (pdf:get-font *default-font-name*)))
#+nil	(boxed-text  "xx xxx xxx xx xxx xxx iii iii  etcetara and etcetera")

#+nnil	(let ((text (cl-typesetting:compile-text (:font "URWPalladioL-Roma" :font-size 12.0)
		      "AVERY Qq and now something else")))
	  (typeset::draw-block text 100 100 300 50))
#+nil	(let ((text (cl-typesetting:compile-text (:font "URWPalladioL-Roma" :font-size 12.0)
		      "AVERY Qq and now something else kasj dfjas as f asf asf s f asf as f asf as fsa dfsd sf sf as fsf  sf as fas f saf as f af as fas f as fs f -- a --")))
	  (typeset::draw-block text 100 100 100 1000))
	(let* (#+nil (subcontent  (cl-typesetting::fit-lines (cl-typesetting:compile-text (:font "Times-Roman" :font-size 8) "Hallo" :eol "Daar") 500 200))
	       (text (cl-typesetting:compile-text (:font "Courier" :font-size 18)
		       "Hallo"
		       (let ((subcontent  (cl-typesetting::fit-lines (cl-typesetting:compile-text (:font "Times-Roman" :font-size 8) "Hallo" :eol "Daar") 500 200)))
			 (cl-typesetting::add-box (make-instance 'boxed-content  :boxes  subcontent))
			 (cl-typesetting::do-layout subcontent))
		       
		       :eol "En" :eol "nog" :eol  "een" :eol
		       "AVAVAVAVAVAVAVAVAVAV"))
	       (lines  (cl-typesetting::fit-lines text 10000 10000))
	       (box (make-instance #+nil 'cl-typesetting::vbox 'boxed-content :boxes lines)))
;	  (inspect lines)
	  (cl-typesetting::do-layout box)
	  
	  (cl-typesetting::stroke box 0 130))
#+nil	(pdf:in-text-mode
	  (pdf:set-font font-1 18.0)
	  (pdf:move-text 100 120)
	  (pdf:draw-text "(Hallo) (Daar-)")
;	  (pdf:move-text 0 -20)
;	  (pdf:set-font font-2 12.0)
;	  (pdf:draw-text "AVERY Qq and now something else")
	  (pdf:move-text 0 -20)
	  (pdf:draw-spaced-strings (kerned-text "AVERY Qq and now something else"))
	  )
	)
      (setf (cl-pdf:bounds cl-pdf:*page*) #(-20 -200 200 200)))
    
    (pdf:write-document file-name)))


(defun make-serie (fn from to step)
  (loop :for x :from from :to to :by step
	:collect (list x (funcall fn x))))

(defun test-chart (&optional (file-name #P "/tmp/test-2.pdf"))
  (pdf:with-document ()
    (pdf:with-page (:bounds #(0 0 650 400))
      (pdf:draw-object
       (make-instance 'pdf:plot-xy :x 50 :y 50 :width 500 :height 300
				   :title "Hallo Chart"
				   :labels&colors '(("S()erie 1" (1.0 0.0 0.0))
						    ("Nr 2" (0.0 1.0 0.0)))
				   :series (list (make-serie #'sin 0 5 0.1)
						 (make-serie #'exp 0 5 0.1))
				   :point-radius 0
				   :line-width 1)))
    (pdf:write-document file-name)))



;;;; experiment

 (cl-typesetting::compile-text () "Hallo" "Daar" :eol "Etcetera en zo")
