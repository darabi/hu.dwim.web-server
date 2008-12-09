;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Graph component

(def special-variable *stroke-width* 2.5)

(def special-variable *graph-inset* 5)

(def special-variable *vertex-inset* 5)

(def component graph-component ()
  ((graph)))

(def render graph-component ()
  (bind ((graph (graph-of -self-))
         (inset-string (princ-to-string *graph-inset*)))
    (dmm::layout-graph graph)
    (flet ((marker (id &key path stroke stroke-width fill refX refY)
             <svg:marker (:id ,id :orient "auto" :stroke ,stroke :stroke-width ,stroke-width :fill ,fill
                          :viewBox "0 0 10 10" :refX ,refX :refY ,refY
                          :markerUnits "strokeWidth" :markerWidth 10 :markerHeight 5)
                         <svg:path (:d ,path)>>))
      <embed (:width ,(+ (* 2 *graph-inset*) (dmm::width-of graph))
              :height ,(+ (* 2 *graph-inset*) (dmm::height-of graph))
              :type "image/svg+xml"
              :src ,(action/href (:delayed-content #t)
                      (make-buffered-functional-html-response ((+header/content-type+ +svg-xml-mime-type+))
                        <svg:svg (:xmlns:svg "http://www.w3.org/2000/svg" :version "1.2")
                                 <svg:g (:transform ,(concatenate-string "translate(" inset-string "," inset-string ")"))
                                        <svg:defs
                                            ;; TODO fuck SVG and firefox! for not being able to draw markers intentionally pointing into the right direction
                                            ,(progn
                                              (marker "normal-arrow-start" :path "M 10 0 L 0 5 L 10 10 z" :refX 8 :refY 5)
                                              (marker "normal-arrow-end" :path "M 0 0 L 10 5 L 0 10 z" :refX 0 :refY 5)
                                              (marker "empty-arrow-start" :path "M 10 0 L 0 5 L 10 10 z" :refX 8 :refY 5 :stroke-width 1 :stroke "black" :fill "white")
                                              (marker "empty-arrow-end" :path "M 0 0 L 10 5 L 0 10 z" :refX 0 :refY 5 :stroke-width 1 :stroke "black" :fill "white")
                                              (marker "reverse-arrow-with-line-start" :path "M 10 5 L 0 0 L 0 10 z M 10 5 L 0 5" :refX 8 :refY 5 :stroke "black" :stroke-width 1 :fill "white")
                                              (marker "reverse-arrow-with-line-end" :path "M 0 5 L 10 0 L 10 10 z M 0 5 L 10 5" :refX 0 :refY 5 :stroke "black" :stroke-width 1 :fill "white")
                                              (marker "filled-diamond-start" :path "M 5 0 L 0 5 L 5 10 L 10 5 z" :refX 8 :refY 5)
                                              (marker "filled-diamond-end" :path "M 5 0 L 10 5 L 5 10 L 0 5 z" :refX 0 :refY 5))>
                                        ,(let ((%graph-height% (dmm::height-of graph)))
                                              (declare (special %graph-height%))
                                              (cl-graph::iterate-edges graph #'render-edge)
                                              (cl-graph::iterate-nodes graph #'render-vertex))>>)))>)))

;; converts to physical SVG coordinates
(def function svg-y (y)
  (declare (special %graph-height%))
  (- %graph-height% y))

(def function render-vertex (vertex)
  (with-slots (dmm::x dmm::y dmm::width dmm::height dmm::content) vertex
    <svg:rect (:x ,dmm::x :y ,(svg-y (+ dmm::y dmm::height))
               :rx 6 :ry 6 :width ,dmm::width :height ,dmm::height
               :fill "rgb(224,224,255)" :stroke "blue" :stroke-width ,*stroke-width*)>
    (svg-stroke (dmm::compiled-content-of vertex)
                (+ *vertex-inset* dmm::x)
                (- (+ dmm::y dmm::height) *vertex-inset*))))

(def function render-edge (edge)
  (flet ((p->string (p)
           (concatenate-string (princ-to-string (first p)) "," (princ-to-string (svg-y (second p))))))
    (bind ((bezier-points (dmm::points-of edge))
           (points-length (length bezier-points)))
      (unless (zerop points-length)
        (iter (for p1 first (car bezier-points) then p4)
              (for (p2 p3 p4 more) on (cdr bezier-points) by #'cdddr)
              (for i :from 1)
              <svg:path (:d ,(concatenate-string "M" (p->string p1)
                                                 " C" (p->string p2)
                                                 " " (p->string p3)
                                                 " " (p->string p4))
                         :marker-end ,(when (= i (/ (1- points-length) 3))
                                            (arrow-marker (dmm::head-arrow-of edge) "end"))
                         :marker-start ,(when (first-iteration-p)
                                              (arrow-marker (dmm::tail-arrow-of edge) "start"))
                         :fill "none" :stroke "brown" :stroke-width ,*stroke-width*) >)))))

(def function arrow-marker (arrow type)
  (when arrow
    (concatenate-string "url(#" (string-downcase (symbol-name (dmm::shape-of arrow))) "-" type ")")))

(def function emit-tspan (elements)
  (when elements
    (if (stringp (car elements))
        (progn
          `xml,(car elements)
          (emit-tspan (cdr elements)))
        ;; TODO: is it really 72?
        <svg:tspan (:dx ,(coerce (- (/ (car elements) 72)) 'float) :fill ,(svg-color typeset::*color*))
                   ,(emit-tspan (cdr elements))>)))

(def function svg-color (color)
  (concatenate-string "rgb("
                      (princ-to-string (round (* 255 (first color)))) ","
                      (princ-to-string (round (* 255 (second color)))) ","
                      (princ-to-string (round (* 255 (third color)))) ")"))

(def generic svg-stroke (box x y)
  (:method (box x y)
    )

  (:method :before ((box typeset::char-box) x y)
    (when (functionp typeset::*pre-decoration*) 
      (funcall typeset::*pre-decoration*
               box
               x (+ y (typeset::baseline box) (typeset::offset box))
               (typeset::dx box) (- (typeset::dy box)))))

  (:method :after ((box typeset::char-box) x y)
    (when (functionp typeset::*post-decoration*)
      (funcall typeset::*post-decoration*
               box
               x (+ y (typeset::baseline box) (typeset::offset box))
               (typeset::dx box) (- (typeset::dy box)))))

  (:method ((box typeset::hrule) x y)
    (if (typeset::stroke-fn box)
        (funcall (typeset::stroke-fn box) box x y)
        (unless (zerop (typeset::dy box))
          <svg:rect (:x ,x :y ,(svg-y y) :width ,(typeset::dx box) :height ,(typeset::dy box) :color ,(svg-color typeset::*color*))>)))

  (:method ((hbox typeset::hbox) x y)
    (decf x (typeset::baseline hbox))
    (decf x (typeset::offset hbox))
    (decf y (typeset::internal-baseline hbox))
    (dolist (box (typeset::boxes hbox))
      (svg-stroke box x y)
      (incf x (+ (typeset::dx box) (typeset::delta-size box)))))

  (:method ((hbox typeset::hbox) x y)
    (decf x (typeset::baseline hbox))
    (decf x (typeset::offset hbox))
    (decf y (typeset::internal-baseline hbox))
    (dolist (box (typeset::boxes hbox))
      (svg-stroke box x y)
      (incf x (+ (typeset::dx box) (typeset::delta-size box)))))

  (:method ((vbox typeset::vbox) x y)
    (incf y (typeset::baseline vbox))
    (incf y (typeset::offset vbox))
    (incf x (typeset::internal-baseline vbox))
    (dolist (box (typeset::boxes vbox))
      (svg-stroke box x y)
      (decf y (+ (typeset::dy box) (typeset::delta-size box)))))

  (:method ((box typeset::char-box) x y)
    `xml,(typeset::boxed-char box))

  (:method ((line typeset::text-line) x y)
    (decf y (typeset::internal-baseline line))
    (let ((string ())
          (offset 0)
          (nb-spaces 0)
          text-x text-y
          (text-chunk ()))
      (labels ((end-string ()
                 (when string
                   (push (coerce (nreverse string) typeset::unicode-string-type) text-chunk)
                   (setf string nil)))
               (end-text-chunk ()
                 (end-string)
                 (setf nb-spaces 0)
                 (when (some 'stringp text-chunk) 
                   ;; (print (nreverse text-chunk))
                   <svg:text (:x ,text-x
                              :y ,(svg-y text-y)
                              :fill ,(svg-color typeset::*color*)
                              :font-family ,(slot-value typeset::*font* 'pdf:name)
                              :font-size ,typeset::*font-size*)
                             ,(emit-tspan (nreverse text-chunk))>
		 (setf text-chunk nil)))
	     (add-char (char-box)
	       (when (/= offset (typeset::offset char-box))
		 (end-text-chunk)
		 (setf offset (typeset::offset char-box)
		       text-y (+ offset y)))
	       (unless (or string text-chunk)
		 (setf text-x x
                       text-y (+ offset y)))
	       (push (typeset::boxed-char char-box) string))
	     (add-spacing (space)
	       (setf space (round (/ (* -1000 space) typeset::*text-x-scale*) typeset::*font-size*))
	       (unless (zerop space)
		 (end-string)
		 (incf nb-spaces)
                 (when (> nb-spaces 10)
                   (end-text-chunk))
		 (when (or string text-chunk)
		   (push space text-chunk)))))
      (loop for box in (typeset::boxes line)
	    for size = (+ (typeset::dx box) (typeset::delta-size box))
	    do
	    (cond
	      ((or (functionp typeset::*pre-decoration*)
		   (functionp typeset::*post-decoration*))
	       (end-text-chunk)
	       (svg-stroke box x y))
	      ((typeset::char-box-p box) (add-char box))
	      ((typeset::white-space-p box) (add-spacing size))
	      (t (end-text-chunk) (svg-stroke box x y)))
	    (incf x size))
      (end-text-chunk))))

  (:method ((style typeset::text-style) x y)
    (when (typeset::font style)
      (setf typeset::*font* (typeset::font style)))
    (when (typeset::font-size style)
      (setf typeset::*font-size* (typeset::font-size style)))
    (when (typeset::text-x-scale style)
      (setf typeset::*text-x-scale* (typeset::text-x-scale style)))
    (when (typeset::color style)
      (setf typeset::*color* (typeset::color style)))
    (when (typeset::pre-decoration style)
      (setf typeset::*pre-decoration* (typeset::pre-decoration style)))
    (when (typeset::post-decoration style)
      (setf typeset::*post-decoration* (typeset::post-decoration style)))))
