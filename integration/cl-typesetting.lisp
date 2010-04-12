;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Font loading

(def function get-font-path-list (directory font-extension metrics-extension)
  (when (cl-fad:directory-exists-p directory)
    (prog1-bind file-names nil
      (cl-fad:walk-directory directory (lambda (path)
                                         (bind ((type (pathname-type path)))
                                           (when (and type (string= type metrics-extension))
                                             (push (merge-pathnames (make-pathname :type font-extension) path) file-names))))))))

(def function load-truetype-unicode-font (font-path)
  (pdf:load-ttu-font
   (namestring (merge-pathnames (make-pathname :type "ufm") font-path))
   (namestring (merge-pathnames (make-pathname :type "ttf") font-path))))

(def function load-type1-font (font-path)
  (pdf:load-t1-font
   (namestring (merge-pathnames (make-pathname :type "afm") font-path))
   (namestring (merge-pathnames (make-pathname :type "pfb") font-path))))

(def function load-fonts ()
  (let ((font-directory (system-relative-pathname :hu.dwim.wui "font/")))
    (dolist (ttf-font-path (get-font-path-list font-directory "ttf" "ufm"))
      (wui.info "Loading truetype unicode font ~A." ttf-font-path)
      (load-truetype-unicode-font ttf-font-path))
    (dolist (t1-font-path (get-font-path-list font-directory "pfb" "afm"))
      (wui.info "Loading type1 font ~A." t1-font-path)
      (load-type1-font t1-font-path))))

(load-fonts)

;;;;;;
;;; Export

(def (special-variable e) *total-page-count*)

(def layered-method export-pdf ((self exportable/abstract))
  (with-output-to-export-stream (*pdf-stream* :content-type +pdf-mime-type+ :external-format :iso-8859-1)
    (bind ((typeset::*default-font* (pdf:get-font "FreeSerif"))
           (typeset::*font* typeset::*default-font*)
           (*total-page-count* 0))
      (typeset::with-document ()
        (render-pdf-pages self)
        (when pdf:*page*
          (typeset:finalize-page pdf:*page*))
        (typeset:write-document *pdf-stream*)))))

(def (layered-function e) render-pdf-pages (component)
  (:method ((self component))
    (typeset:draw-pages (typeset:compile-text ()
                          (with-active-layers (passive-layer)
                            (render-pdf self)))
                        :margins '(72 72 72 50)
                        :header (render-pdf-header self)
                        :footer (render-pdf-footer self))))

(def (layered-function e) render-pdf-header (component)
  (:method ((self component))
    (bind ((component-value (component-value-of self)))
      (lambda (page)
        (declare (ignore page))
        (unless (and (= pdf:*page-number* 1)
                     (typep component-value 'book))
          (typeset:compile-text ()
            (typeset:paragraph (:font "FreeSerif" :font-size 12)
              (typeset:hbox (:align :center :adjustable-p #t)
                (when (typep component-value 'title-mixin)
                  (typeset:put-string (title-of component-value)))
                :hfill
                (typeset:put-string (hu.dwim.wui::localized-timestamp (local-time:now))))
              (typeset:hrule :dy 1/2))))))))

(def layered-function render-pdf-footer (component)
  (:method ((self component))
    (bind ((component-value (component-value-of self)))
      (lambda (page)
        (declare (ignore page))
        (unless (and (= pdf:*page-number* 1)
                     (typep component-value 'book))
          (typeset:compile-text ()
            (typeset:paragraph (:font "FreeSerif" :font-size 12 :h-align :center)
              (setf *total-page-count* (max *total-page-count* pdf:*page-number*))
              (typeset:put-string (format nil "~d / ~d oldal" pdf:*page-number* *total-page-count*)))))))))

;;;;;;
;;; Render pdf

(def render-pdf string ()
  (typeset:put-string -self-))

(def render-pdf command/widget
  (render-component (content-of -self-)))

(def render-pdf command-bar/widget
  (iter (for command :in (commands-of -self-))
        (unless (first-iteration-p)
          (typeset:put-string " "))
        (render-component command)))

(def render-pdf popup-menu/widget
  (iter (for command :in (commands-of -self-))
        (unless (first-iteration-p)
          (typeset:put-string " "))
        (render-component command)))

(def render-pdf list/widget ()
  (foreach #'render-component (contents-of -self-)))

(def render-pdf element/widget ()
  (render-component (content-of -self-)))

(def render-pdf table/widget ()
  (typeset:table (:col-widths (normalized-column-widths (columns-of -self-)) :splittable-p #t)
    (typeset:row ()
      (foreach #'render-component (columns-of -self-)))
    (foreach #'render-component (rows-of -self-))))

(def render-pdf column/widget ()
  (typeset:cell ()
    (render-header-for -self-)))

(def render-pdf cell/widget ()
  (bind (((:read-only-slots horizontal-alignment vertical-alignment column-span row-span) -self-))
    ;; TODO handle word-wrap slot
    (typeset:cell (:v-align (or vertical-alignment :top)
                   :col-span (or column-span 1)
                   :row-span (or row-span 1))
      (surround-body-when horizontal-alignment
          (typeset:paragraph (:h-align horizontal-alignment)
            (-body-))
        (render-content-for -self-)))))

(def render-pdf row/widget ()
  (typeset:row ()
    (render-table-row-cells (parent-component-of -self-) -self-)))

(def layered-method render-table-row-cell :in pdf-layer ((table table/widget) (row row/widget) (column column/widget) (cell component))
  (typeset:cell ()
    (render-component cell)))

(def layered-method render-table-row-cell :in pdf-layer ((table table/widget) (row row/widget) (column column/widget) (cell string))
  (typeset:cell ()
    (render-component cell)))

(def render-pdf tree/widget ()
  (foreach #'render-component (root-nodes-of -self-)))

(def render-pdf node/widget ()
  (foreach #'render-component (child-nodes-of -self-)))

(def render-pdf treeble/widget ()
  (bind ((columns (columns-of -self-)))
    (typeset:table (:col-widths (normalized-column-widths columns) :splittable-p #t)
      (typeset:row ()
        (foreach #'render-component columns))
      (foreach #'render-component (root-nodes-of -self-)))))

(def render-pdf nodrow/widget ()
  (typeset:row ()
    (render-nodrow-cells -self-))
  (foreach #'render-component (child-nodes-of -self-)))

(def render-pdf book/text/inspector
  (typeset:paragraph (:font-size 24 :h-align :center)
    (typeset:vspace 320)
    (render-title-for -self-)
    (typeset:new-page))
  (render-component (toc-of -self-))
  (typeset:paragraph (:font "FreeSerif")
    (foreach #'render-author (authors-of (component-value-of -self-)))
    (typeset:new-line)
    (render-contents-for -self-)))

(def render-pdf book/toc/inspector
  (typeset:paragraph (:font "FreeSerif" :font-size 14 :h-align :center)
    "Tartalomjegyzék"
    (typeset:vspace 10))
  (render-contents-for -self-)
  #+nil
  (iter (for chapter :in '(((1))))
        (for depth = (length (first chapter)))
        (typeset:paragraph (:h-align :left-but-last
                            :left-margin (case depth (1 0) (2 10) (t 20))
                            :top-margin (case depth (1 3) (t 0))
                            :bottom-margin (case depth (1 2) (t 0))
                            :font-size (case depth (1 12) (2 10) (t 9))
                            :font "FreeSerif")
          (typeset:put-string "Chapter 1")
          (typeset::dotted-hfill)
          (typeset:with-style (:font-size 10 :font "FreeSerif")
            (typeset::put-ref-point-page-number 1))))
  (typeset:new-page))

(def render-pdf chapter/toc/inspector
  (bind (((:read-only-slots numbering) -self-)
         (depth (1+ (count #\. numbering))))
    (typeset:paragraph (:h-align :left-but-last
                        :left-margin (case depth (1 0) (2 10) (t 20))
                        :top-margin (case depth (1 3) (t 0))
                        :bottom-margin (case depth (1 2) (t 0))
                        :font-size (case depth (1 12) (2 10) (t 9))
                        :font "FreeSerif")
      (render-component (reference-of -self-))
      (typeset::dotted-hfill)
      (typeset::put-ref-point-page-number numbering)))
  (render-contents-for -self-))

(def render-pdf chapter/text/inspector
  (bind ((numbering (toc-numbering -self-)))
    (typeset:mark-ref-point numbering)
    (typeset:paragraph (:font-size 18)
      (render-component numbering)
      (typeset:put-string " ")
      (render-title-for -self-)
      (typeset:new-line))
    (typeset:paragraph ()
      (render-contents-for -self-))))

(def render-pdf paragraph/text/inspector
  (typeset:paragraph ()
    (render-contents-for -self-)
    (typeset:vspace 6)))

(def render-pdf title/widget
  (typeset:paragraph (:font-size 14)
    (render-content-for -self-)))

(def render-pdf hyperlink/text/inspector
  ;; TODO: how does one render a link?
  (render-content-for -self-))

(def render-pdf shell-script/text/inspector
  (typeset:with-style (:font "Courier" :font-size 12)
    (iter (for content :in (contents-of -self-))
          (typeset:new-line)
          (render-component content))))

(def render-pdf uri/external-link/inspector
  (typeset:put-string (print-uri-to-string (component-value-of -self-))))

;;;;;;
;;; Utilities

(def generic pdf-column-width (column)
  (:method ((column column/widget))
    ;; TODO: KLUDGE: eh?!
    100))

(def function normalized-column-widths (columns)
  (bind ((column-widths (mapcar 'pdf-column-width columns))
         (total-width (sum* column-widths)))
    (mapcar (lambda (width)
              ;; TODO: consider page size and orientation 725 for landscape, 430 for portrait
              (* 430 (coerce (/ width total-width) 'double-float)))
            column-widths)))
























;;;;;;
;;; graph/widget

(def special-variable *vertex-inset* 5)

(def special-variable *dpi* 72.0)

(def special-variable *max-vertex-width* 200)

(def special-variable *arrow-width* 6)

(def special-variable *arrow-length* 10)

(def special-variable *edge-label-font* "FreeSerif")

(def special-variable *edge-label-font-size* 11)

(def special-variable *vertex-label-font* "FreeSerif")

(def special-variable *vertex-label-font-size* 11)

(def render-pdf graph/widget
  (bind (((:slots x y width height max-width max-height scale) -self-))
    (cl-graph:layout-graph-with-graphviz -self-)
    (typeset:user-drawn-box :inline #t
                            :stroke-fn (lambda (box x y)
                                         (declare (ignore box))
                                         (stroke-graph -self- x y))
                            :dx (* scale width) :dy (* scale height))))

(def function stroke-graph (graph x y)
  (pdf:with-saved-state
    (pdf:set-color-fill '(1 1 1))
    (pdf:translate x y)
    (pdf:scale (scale-of graph) (scale-of graph))
    (pdf:translate (- (x-of graph)) (- (+ (y-of graph) (height-of graph))))
    (when (border-width-of graph)
      (pdf:set-color-stroke '(0 0 0))
      (pdf:set-line-width (border-width-of graph))
      (pdf:basic-rect (x-of graph) (y-of graph) (width-of graph) (height-of graph))
      (pdf:fill-and-stroke))
    (cl-graph:iterate-edges graph 'stroke-edge)
    (cl-graph:iterate-vertexes graph 'stroke-vertex)))

(def function stroke-vertex (vertex)
  (pdf:with-saved-state
    (pdf:set-color-fill '(1 1 1))
    (when (border-width-of vertex)
      (pdf:set-color-stroke '(0 0 0))
      (pdf:set-line-width (border-width-of vertex))
      (pdf:basic-rect (x-of vertex)
                      (y-of vertex)
                      (width-of vertex)
                      (height-of vertex))
      (pdf:fill-and-stroke)))
  (typeset::stroke (typeset:make-filled-vbox (typeset:compile-text ()
                                               (typeset:paragraph (:font "FreeSerif" :color '(0 0 0))
                                                 (render-pdf (content-of vertex))))
                                             (width-of vertex) typeset::+HUGE-NUMBER+)
                   (+ *vertex-inset* (x-of vertex))
                   (+ (- *vertex-inset*) (y-of vertex) (height-of vertex))))

(def function stroke-edge (edge)
  (pdf:with-saved-state
    (pdf:set-color-stroke '(0 0 0))
    (pdf:set-line-width (width-of edge))
    (let ((points (points-of edge))
          x1 y1 x2 y2 x3 y3 prev-x1 prev-y1)
      (when points
        (pdf:move-to (caar points) (second (pop points)))
        (iter (while points)
              (setf prev-x1 x1 prev-y1 y1)
              (setf x1 (caar points) y1 (second (pop points))
                    x2 (caar points) y2 (second (pop points))
                    x3 (caar points) y3 (second (pop points)))
              (assert (and x1 y1 x2 y2 x3 y3))
              (pdf:bezier-to x1 y1 x2 y2 x3 y3))
        (pdf:stroke)
        (setf points (points-of edge))
        (awhen (tail-arrow-of edge)
          (stroke-arrow it (caaddr points) (car (cdaddr points)) (caar points) (cadar points)))
        (awhen (head-arrow-of edge)
          (stroke-arrow it x1 y1 x3 y3))
        (stroke-label edge)))))

(def function stroke-arrow (arrow x1 y1 x2 y2)
  (when arrow
    (bind ((nx (- x1 x2))
           (ny (- y1 y2))
           (l (sqrt (+ (* nx nx)(* ny ny))))
           (x0)
           (y0)
           (shape (shape-of arrow))
           (reverse-arrow-with-line-p (eq shape :reverse-arrow-with-line))
           (arrow-length (if reverse-arrow-with-line-p
                             (- *arrow-length*)
                             *arrow-length*)))
      (setf nx (/ nx l)
            ny (/ ny l))
      (unless reverse-arrow-with-line-p
        (decf x2 (* nx arrow-length))
        (decf y2 (* ny arrow-length)))
      (pdf:move-to x2 y2)
      (setf x0 (+ x2 (* nx arrow-length))
            y0 (+ y2 (* ny arrow-length))
            nx (* nx *arrow-width*)
            ny (* ny *arrow-width*))
      (pdf:line-to (+ x0 ny) (- y0 nx))
      (pdf:line-to (- x0 ny) (+ y0 nx))
      (pdf:line-to x2 y2)
      (when reverse-arrow-with-line-p
        (pdf:line-to x0 y0))
      (if (eq (shape-of arrow) :filled-arrow)
          (pdf:fill-and-stroke)
          (pdf:stroke)))))

;; TODO: handle label coordinates
(def function stroke-label (edge)
  (when (label-of edge)
    (bind ((points (points-of edge))
           (first-point (first points))
           (last-point (lastcar points))
           (x (/ (+ (first first-point) (first last-point)) 2))
           (y (/ (+ (second first-point) (second last-point)) 2)))
      (pdf:set-color-fill '(0 0 0))
      (pdf:draw-centered-text x y (label-of edge)
                              (pdf:get-font *edge-label-font*) *edge-label-font-size*))))

#+nil
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
      (wui.debug "Precalculated vertex size for ~A is (~A, ~A)" vertex width height)
      (setf (getf (cl-graph:dot-attributes vertex) :width) (/ (+ (* 2 *vertex-inset*) width) *dpi*))
      (setf (getf (cl-graph:dot-attributes vertex) :height) (/ (+ (* 2 *vertex-inset*) height) *dpi*)))))

(def function center-x-of (vertex)
  (+ (x-of vertex) (/ (width-of vertex) 2.0)))

(def function center-y-of (vertex)
  (+ (y-of vertex) (/ (height-of vertex) 2.0)))

(def function push-dot-attribute (object key value)
  (push value (cl-graph:dot-attributes object))
  (push key (cl-graph:dot-attributes object)))
