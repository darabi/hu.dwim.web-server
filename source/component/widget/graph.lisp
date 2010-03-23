;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; graph/widget

(def (component e) graph/widget (widget/style cl-graph:dot-graph)
  ((x :type number)
   (y :type number)
   (width :type number)
   (height :type number)
   (scale 1 :type number)
   (max-width 400 :type number)
   (max-height 400 :type number)
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

(def render-text graph/widget
  (render-component "Graph omitted from text output."))

(def render-xhtml graph/widget ()
  (bind (((:read-only-slots width height style-class custom-style id) -self-))
    <div (:id ,id :class ,style-class :style `str(,custom-style "position: relative; width: " ,(integer-to-string width) "px; height: " ,(integer-to-string height) "px;"))
      <embed (:style `str("position: absolute;")
              :width ,width
              :height ,height
              :type "image/svg+xml"
              :src ,(action/href (:delayed-content #t)
                      (make-buffered-functional-html-response ((+header/content-type+ +svg-xml-mime-type+))
                        (with-active-layers (xhtml-layer)
                          <svg:svg (:xmlns:svg "http://www.w3.org/2000/svg" :version "1.2")
                            <svg:defs
                              ;; TODO fuck SVG and firefox! for not being able to draw markers intentionally pointing into the right direction
                              ,(flet ((marker (id &key path stroke stroke-width fill refX refY)
                                        <svg:marker (:id ,id :orient "auto" :stroke ,stroke :stroke-width ,stroke-width :fill ,fill
                                                     :viewBox "0 0 10 10" :refX ,refX :refY ,refY
                                                     :markerUnits "strokeWidth" :markerWidth 10 :markerHeight 5)
                                          <svg:path (:d ,path)>>))
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
                               (values))>))))>
      <div (:style "position: absolute;")
        ,(bind ((%graph-height% height))
           (declare (special %graph-height%))
           (cl-graph::iterate-nodes -self- #'render-component))>>))

(def render-odt graph/widget
  <text:p "Not yet implemented">)

(def function add-vertices-and-edges (graph vertices-and-edges)
  (dolist (vertice-or-edge vertices-and-edges)
    (etypecase vertice-or-edge
      (vertex/widget
        (cl-graph:add-vertex graph vertice-or-edge))
      (edge/widget
        (bind ((vertices (collect-if (of-type 'vertex/widget) vertices-and-edges)))
          (setf (slot-value vertice-or-edge 'cl-graph:vertex-1) (find (cl-graph:vertex-1 vertice-or-edge) vertices :key #'cl-graph:vertex-id))
          (setf (slot-value vertice-or-edge 'cl-graph:vertex-2) (find (cl-graph:vertex-2 vertice-or-edge) vertices :key #'cl-graph:vertex-id))
          (cl-graph:add-edge graph vertice-or-edge))))))

;;;;;;
;;; vertex/widget

(def (component e) vertex/widget (widget/style cl-graph:dot-vertex)
  ((x :type number)
   (y :type number)
   (width :type number)
   (height :type number)
   (shape :box :type (member :box))
   (border-width 1 :type number)
   (content nil :type component)))

(def constructor vertex/widget
  (setf (slot-value -self- 'cl-graph:element) (cl-graph:vertex-id -self-)))

(def (macro e) vertex/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'vertex/widget ,@args :content ,(the-only-element content)))

(def method component-style-class ((self vertex/widget))
  (string+ "content-border " (call-next-method)))

(def render-xhtml vertex/widget
  (bind (((:read-only-slots x y width height content style-class custom-style id) -self-))
    <div (:id ,id :class ,style-class
          :style `str(,custom-style "position: absolute; "
                      "left: " ,(princ-to-string x) "px; "
                      "top: " ,(princ-to-string (svg-y (+ y height))) "px; "
                      "width: " ,(princ-to-string (- width 10)) "px; "
                      "height: " ,(princ-to-string (- height 10)) "px;"))
      ,(render-component content)>))

(def generic compute-vertex-size (vertex content)
  (:method (vertex (content (eql nil)))
    (values))

  (:method (vertex content)
    (setf (getf (cl-graph:dot-attributes vertex) :width) 1)
    (setf (getf (cl-graph:dot-attributes vertex) :height) 1)))

;;;;;;
;;; edge/widget

(def (component e) edge/widget (widget/basic cl-graph:dot-edge)
  ((points :type list)
   (width 1 :type number)
   (label nil :type component)
   (label-x :type number)
   (label-y :type number)
   (head-arrow nil :type arrow/widget)
   (tail-arrow nil :type arrow/widget)))

(def (macro e) edge/widget ((&rest args &key &allow-other-keys) &body label)
  `(make-instance 'edge/widget ,@args :label ,(the-only-element label)))

(def render-xhtml edge/widget
  (flet ((p->string (p)
           (string+ (princ-to-string (first p)) "," (princ-to-string (svg-y (second p)))))
         (arrow-marker (arrow type)
           (when arrow
             (string+ "url(#" (string-downcase (symbol-name (shape-of arrow))) "-" type ")"))))
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
                         :fill "none" :stroke "brown" :stroke-width 2)>)))))

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
