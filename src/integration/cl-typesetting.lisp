;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Export

(def (special-variable e) *total-page-count*)

(def layered-method execute-export-pdf ((component component))
  (bind ((typeset::*default-font* (pdf:get-font "Times-Roman"))
         (typeset::*font* typeset::*default-font*)
         (*total-page-count* 0))
    (typeset::with-document ()
      (render-pdf-pages component)
      (when pdf:*page*
        (typeset:finalize-page pdf:*page*))
      (typeset:write-document *pdf-stream*))))

(def (layered-function e) render-pdf-pages (component)
  (:method ((component component))
    (typeset:draw-pages (typeset:compile-text ()
                          (render-pdf component))
                        :margins '(72 72 72 50)
                        :header (render-pdf-header component)
                        :footer (render-pdf-footer component))))

;;;;;;
;;; Render pdf

(def render-pdf string ()
  (typeset:put-string -self-))

(def render-pdf command-bar/basic ()
  (iter (for command :in (commands-of -self-))
        (unless (first-iteration-p)
          (typeset:put-string " "))
        (render-component command)))

(def render-pdf popup-menu/basic ()
  (iter (for command :in (commands-of -self-))
        (unless (first-iteration-p)
          (typeset:put-string " "))
        (render-component command)))

(def render-pdf primitive-component ()
  (typeset:put-string (print-component-value -self-)))

(def render-pdf standard-object-detail-component ()
  (typeset:table (:col-widths '(200 200) :splittable-p #t)
    (foreach #'render-component (slot-value-groups-of -self-))))

(def render-pdf standard-object-slot-value-group-component ()
  (foreach (lambda (slot-value)
             (typeset:row ()
               (render-component slot-value)))
           (slot-values-of -self-)))

(def render-pdf standard-object-slot-value/inspector ()
  (typeset:cell ()
    (render-component (label-of -self-)))
  (typeset:cell ()
    (render-component (value-of -self-))))

(def render-pdf table-component ()
  (typeset:table (:col-widths (normalized-column-widths (columns-of -self-)) :splittable-p #t)
    (typeset:row ()
      (foreach #'render-component (columns-of -self-)))
    (foreach #'render-component (rows-of -self-))))

(def render-pdf column-component ()
  (typeset:cell ()
    (call-next-method)))

(def render-pdf cell-component ()
  (bind (((:read-only-slots horizontal-alignment vertical-alignment column-span row-span) -self-))
    ;; TODO handle word-wrap slot
    (typeset:cell (:v-align (or vertical-alignment :top)
                   :col-span (or column-span 1)
                   :row-span (or row-span 1))
      (surround-body-when horizontal-alignment
          (typeset:paragraph (:h-align horizontal-alignment)
            (-body-))
        (call-next-method)))))

(def render-pdf row-component ()
  (typeset:row ()
    (bind ((table (parent-component-of -self-)))
      (foreach (lambda (cell column)
                 (render-cells table -self- column cell))
               (cells-of -self-)
               (columns-of table)))))

(def layered-methods render-cells
  (:method :in pdf-layer ((table table/mixin) (row row/basic) (column column-component) (cell component))
    (typeset:cell ()
      (render-component cell)))

  (:method :in pdf-layer ((table table/mixin) (row row/basic) (column column-component) (cell string))
    (typeset:cell ()
      (render-component cell))))

(def render-pdf tree/basic ()
  (bind ((columns (columns-of -self-)))
    (typeset:table (:col-widths (normalized-column-widths columns) :splittable-p #t)
      (typeset:row ()
        (foreach #'render-component columns))
      (foreach #'render-component (root-nodes-of -self-)))))

(def render-pdf node/basic ()
  (typeset:row ()
    (foreach (lambda (column cell)
               (render-pdf-tree-cell *tree* -self- column cell))
             (columns-of *tree*)
             (cells-of -self-)))
  (foreach #'render-component (child-nodes-of -self-)))

(def (layered-function e) render-pdf-tree-cell (tree node column cell)
  (:method :before ((tree tree/basic) (node node/basic) (column column-component) (cell cell-component))
    (ensure-refreshed cell))

  (:method ((tree tree/basic) (node node/basic) (column column-component) (cell component))
    (typeset:cell ()
      (render-component cell)))

  (:method ((tree tree/basic) (node node/basic) (column column-component) (cell string))
    (typeset:cell ()
      (render-component cell)))
  
  (:method ((tree tree/basic) (node node/basic) (column column-component) (cell cell-component))
    (render-component cell)))

;;;;;;
;;; Utilities

(def (layered-function e) render-pdf-header (component)
  (:method ((component component))
    (values)))

(def (layered-function e) render-pdf-footer (component)
  (:method ((component component))
    (values)))

(def (function e) digest->bar-code (digest)
  (iter (with bar-code = 0)
        (for index :from (1- (length digest)) :downto 0)
        (for digest-byte :in-vector digest)
        (setf (ldb (byte 8 (* 8 index)) bar-code) digest-byte)
        (finally (return bar-code))))

(def (function e) render-pdf-bar-code (bar-code box x y)
  (pdf:draw-bar-code128 (format nil "~10,'0d" bar-code) x y
                        :width (typeset::dx box) :height (typeset::dy box) :font-size 6 :start-stop-factor 0.4 :segs-per-char 6.5))

(def (function e) render-pdf-dots (count)
  (typeset:put-string (make-array count :initial-element #\. :element-type 'character)))

(def generic pdf-column-width (column)
  (:method ((column column-component))
    ;; TODO: KLUDGE: eh?!
    100))

(def function normalized-column-widths (columns)
  (bind ((column-widths (mapcar 'pdf-column-width columns))
         (total-width (sum column-widths)))
    (mapcar (lambda (width)
              ;; TODO: consider page size and orientation
              (* 725 (coerce (/ width total-width) 'double-float)))
            column-widths)))
