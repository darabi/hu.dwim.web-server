;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; TODO:

(def special-variable *dpi* 72.0)

(def special-variable *max-vertex-width* 200)

(def special-variable *vertex-label-font* "FreeSerif")

(def special-variable *vertex-label-font-size* 11)

(def special-variable *stroke-width* 2.5)

(def special-variable *graph-inset* 5)

(def special-variable *vertex-inset* 5)

;;;;;;
;;; graph/widget

(def (component e) graph/widget (widget/basic
                                 cl-graph:dot-graph)
  ((x :type number)
   (y :type number)
   (width :type number)
   (height :type number)
   (scale 1 :type number)
   (max-width 400 :type number)
   (max-height 400 :type number)
   (background-color '(1.0 1.0 1.0) :type list)
   (border-color '(0.0 0.0 0.0) :type list)
   (border-width nil :type number))
  (:default-initargs
   :vertex-class 'vertex/widget
   :directed-edge-class 'directed-edge/widget
   :undirected-edge-class 'edge/widget))

(def (macro e) graph/widget ((&rest args &key &allow-other-keys) &body vertices-and-edges)
  `(aprog1 (make-instance 'graph/widget ,@args)
     (add-vertices-and-edges it (list ,@vertices-and-edges))))

(def refresh-component graph/widget
  "Layouts the graph using graphviz through CFFI. The graph's coordinate system origin is the bottom left, width and height are increasing right and up."
  (bind (((:slots x y width height max-width max-height scale) -self-))
    (cl-graph::iterate-nodes -self-
                             (lambda (node)
                               ;; TODO: handle node shapes
                               ;; (setf (getf (dot-attributes node) :shape) (shape-of node))
                               (setf (getf (cl-graph:dot-attributes node) :shape) :box)
                               (setf (getf (cl-graph:dot-attributes node) :fixedsize) t)
                               (compute-vertex-size node (content-of node))))
    (cl-graph:iterate-edges -self- [setf (getf (cl-graph:dot-attributes !1) :label) (label-of !1)])
    (cl-graph:layout-graph-with-graphviz -self-)
    ;; store graph coordinates
    (bind ((((blx bly) (urx ury)) (cl-graph:dot-attribute-value :bb -self-)))
      (setf x blx
            y bly
            width (- urx blx)
            height (- ury bly)))
    ;; store edge coordinates
    (cl-graph:iterate-edges -self-
                            (lambda (edge)
                              (setf (points-of edge)
                                    (mapcar [list (first !1) (second !1)]
                                            (cl-graph:dot-attribute-value :pos edge)))))
    ;; store vertex coordinates
    (cl-graph::iterate-nodes -self-
                             (lambda (vertex)
                               (bind (((xc yc) (cl-graph:dot-attribute-value :pos vertex))
                                      (width (coerce (cl-graph:width-in-pixels vertex) 'float))
                                      (height (coerce (cl-graph:height-in-pixels vertex) 'float)))
                                 (setf (x-of vertex) (- xc (/ width 2.0))
                                       (y-of vertex) (- yc (/ height 2.0))
                                       (width-of vertex) width
                                       (height-of vertex) height))))
    ;; make sure the graph will fit
    (when (> (* scale width) max-width)
      (setf scale (/ max-width width)))
    (when (> (* scale height) max-height)
      (setf scale (/ max-height height)))))

(def render-xhtml graph/widget ()
  (bind (((:read-only-slots width height) -self-)
         (inset-string (princ-to-string *graph-inset*)))
    (flet ((marker (id &key path stroke stroke-width fill refX refY)
             <svg:marker (:id ,id :orient "auto" :stroke ,stroke :stroke-width ,stroke-width :fill ,fill
                          :viewBox "0 0 10 10" :refX ,refX :refY ,refY
                          :markerUnits "strokeWidth" :markerWidth 10 :markerHeight 5)
                         <svg:path (:d ,path)>>))
      <embed (:width ,(+ (* 2 *graph-inset*) width)
              :height ,(+ (* 2 *graph-inset*) height)
              :type "image/svg+xml"
              :src ,(action/href (:delayed-content #t)
                      (make-buffered-functional-html-response ((+header/content-type+ +svg-xml-mime-type+))
                        (with-active-layers (xhtml-layer)
                          <svg:svg (:xmlns:svg "http://www.w3.org/2000/svg" :version "1.2")
                            <svg:g (:transform ,(string+ "translate(" inset-string "," inset-string ")"))
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
                              ,(bind ((%graph-height% height))
                                 (declare (special %graph-height%))
                                 (cl-graph::iterate-edges -self- #'render-component)
                                 (cl-graph::iterate-nodes -self- #'render-component))>>))))>)))

(def function add-vertices-and-edges (graph vertices-and-edges)
  (dolist (vertice-or-edge vertices-and-edges)
    (etypecase vertice-or-edge
      (vertex/widget
        (cl-graph:add-vertex graph vertice-or-edge))
      (edge/widget
        (bind ((vertices (filter-out-if (of-type 'vertex/widget) vertices-and-edges)))
          (setf (slot-value vertice-or-edge 'cl-graph:vertex-1) (find (cl-graph:vertex-1 vertice-or-edge) vertices :key #'cl-graph:vertex-id))
          (setf (slot-value vertice-or-edge 'cl-graph:vertex-2) (find (cl-graph:vertex-2 vertice-or-edge) vertices :key #'cl-graph:vertex-id))
          (cl-graph:add-edge graph vertice-or-edge))))))

;;;;;;
;;; vertex/widget

(def (component e) vertex/widget (widget/basic
                                  cl-graph:dot-vertex)
  ((x :type number)
   (y :type number)
   (width :type number)
   (height :type number)
   (shape :box :type (member :box))
   (background-color '(1.0 1.0 1.0) :type list)
   (border-color '(0.0 0.0 0.0) :type list)
   (border-width 1 :type number)
   (content nil :type component)
   (compiled-content nil)
   (tooltip nil :type component)))

(def constructor vertex/widget
  (setf (slot-value -self- 'cl-graph:element) (cl-graph:vertex-id -self-)))

(def (macro e) vertex/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'vertex/widget ,@args :content ,(the-only-element content)))

(def render-xhtml vertex/widget
  (bind (((:read-only-slots x y width height compiled-content) -self-))
    <svg:rect (:x ,x :y ,(svg-y (+ y height))
               :rx 6 :ry 6 :width ,width :height ,height
               :fill "rgb(224,224,255)" :stroke "blue" :stroke-width ,*stroke-width*)>
    (svg-stroke compiled-content
                (+ *vertex-inset* x)
                (- (+ y height) *vertex-inset*))))

;;;;;;
;;; edge/widget

(def (component e) edge/widget (widget/basic
                                cl-graph:dot-edge)
  ((points :type list)
   (line-color '(0.0 0.0 0.0) :type list)
   (width 1 :type number)
   (label nil :type component)
   (label-x :type number)
   (label-y :type number)
   (label-color '(0.0 0.0 0.0) :type list)
   (head-arrow nil :type arrow/widget)
   (tail-arrow nil :type arrow/widget)))

(def (macro e) edge/widget ((&rest args &key &allow-other-keys) &body label)
  `(make-instance 'edge/widget ,@args :label ,(the-only-element label)))

(def render-xhtml edge/widget
  (flet ((p->string (p)
           (string+ (princ-to-string (first p)) "," (princ-to-string (svg-y (second p))))))
    (bind (((:read-only-slots points head-arrow tail-arrow) -self-)
           (bezier-points points)
           (points-length (length bezier-points)))
      (unless (zerop points-length)
        (iter (for p1 first (car bezier-points) then p4)
              (for (p2 p3 p4 more) on (cdr bezier-points) by #'cdddr)
              (for i :from 1)
              <svg:path (:d ,(string+ "M" (p->string p1)
                                                 " C" (p->string p2)
                                                 " " (p->string p3)
                                                 " " (p->string p4))
                         :marker-end ,(when (= i (/ (1- points-length) 3))
                                            (arrow-marker head-arrow "end"))
                         :marker-start ,(when (first-iteration-p)
                                              (arrow-marker tail-arrow "start"))
                         :fill "none" :stroke "brown" :stroke-width ,*stroke-width*)>)))))

;;;;;;
;;; directed-edge/widget

(def (component e) directed-edge/widget (edge/widget cl-graph:dot-directed-edge)
  ())

;;;;;;
;;; arrow/widget

(def (component e) arrow/widget (widget/basic)
  ((shape :type symbol)))

;;;;;;
;;; Util

;; converts to physical SVG coordinates
(def function svg-y (y)
  (declare (special %graph-height%))
  (- %graph-height% y))

(def function arrow-marker (arrow type)
  (when arrow
    (string+ "url(#" (string-downcase (symbol-name (shape-of arrow))) "-" type ")")))

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
  (string+ "rgb("
                      (princ-to-string (round (* 255 (first color)))) ","
                      (princ-to-string (round (* 255 (second color)))) ","
                      (princ-to-string (round (* 255 (third color)))) ")"))

(def generic svg-stroke (box x y)
  (:method (box x y)
    (values))

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
    (bind ((string nil)
           (offset 0)
           (nb-spaces 0)
           text-x text-y
           (text-chunk nil))
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

(def generic compute-vertex-size (vertex content)
  (:method (vertex (content (eql nil)))
    (values))

  (:method (vertex content)
    ;; NOTE: size measurement seems to work in a somewhat bad way
    ;; if you don't know what is going on here, it's better not to change anything
    (bind (box width height)
      ;; first make it as wide as it wants to be
      (unless width
        (setf box (render-vertex-content content))
        (setf width (typeset::compute-boxes-natural-size (typeset::boxes box) #'typeset::dx))
        ;; to calculate the height we have to fit in a box
        (setf box (render-vertex-content content width))
        ;; TODO: WTF 5?
        (setf height (+ 5 (typeset::compute-boxes-natural-size (typeset::boxes box) #'typeset::dy))))
      ;; if it is wider than the maximum, then rewrap the whole thing
      (when (> width *max-vertex-width*)
        (setf box (render-vertex-content content *max-vertex-width*))
        (setf width *max-vertex-width*)
        ;; TODO: WTF 5?
        (setf height (+ 5 (typeset::compute-boxes-natural-size (typeset::boxes box) #'typeset::dy))))
      (when box
        (setf (compiled-content-of vertex) box))
      ;; store sizes in dpi
      (setf (getf (cl-graph:dot-attributes vertex) :width) (/ (+ (* 2 *vertex-inset*) width) *dpi*))
      (setf (getf (cl-graph:dot-attributes vertex) :height) (/ (+ (* 2 *vertex-inset*) height) *dpi*)))))

(def function render-vertex-content (content &optional max-width)
  (bind ((compiled-content
          (cond ((stringp content)
                 (typeset:compile-text ()
                   (typeset:paragraph (:h-align :center
                                       :v-align :center
                                       :color '(0 0 0)
                                       ;; TODO: WTF? parenthesis messes up cl-pdf's output when using FreeSerif font!
                                       :font "Helvetica"
                                       :font-size *vertex-label-font-size*)
                     (typeset:put-string content))))
                ((typep content 'cons)
                 (eval
                  `(compile-text ()
                     ,content)))
                (t
                 content))))
    (if max-width
        (typeset:make-filled-vbox compiled-content max-width typeset::+HUGE-NUMBER+)
        compiled-content)))
