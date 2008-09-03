;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Extended table

(def component extended-table-component (remote-identity-component-mixin)
  ((row-headers nil :type components)
   (row-headers-depth :type integer)
   (row-leaf-count :type integer)
   (column-headers nil :type components)
   (column-headers-depth :type integer)
   (column-leaf-count :type integer)
   (cells nil :type components)))

(def method refresh-component ((self extended-table-component))
  (with-slots (row-headers row-headers-depth row-leaf-count column-headers column-headers-depth column-leaf-count cells) self
    (flet ((setf-indices (headers)
             (iter (for index :from 0)
                   (for leaf :in (collect-leaves headers))
                   (setf (index-of leaf) index))))
      (setf row-headers-depth (count-depth row-headers))
      (setf row-leaf-count (count-leaves row-headers))
      (setf-indices row-headers)
      (setf column-headers-depth (count-depth column-headers))
      (setf column-leaf-count (count-leaves column-headers))
      (setf-indices column-headers))))

(def render extended-table-component ()
  (with-slots (row-headers row-headers-depth row-leaf-count column-headers column-headers-depth column-leaf-count cells) -self-
    (labels ((cell-index (row-path column-path)
               (+ (* column-leaf-count (index-of (last-elt row-path)))
                  (index-of (last-elt column-path))))
             (map-headers-pathes (function headers)
               (map nil (lambda (header)
                          (labels ((traverse (decdendant path)
                                     (aif (children-of decdendant)
                                          (map nil (lambda (child)
                                                     (traverse child (append path (list child))))
                                               it)
                                          (funcall function path))))
                            (traverse header (list header))))
                    headers))
             (render-expanded-command (header)
               (render (command (if (expanded-p header)
                                    "-"
                                    "+")
                                (make-action
                                  (setf (expanded-p header)
                                        (not (expanded-p header)))))))
             (render-top-left-header ()
               <td (:class "header" :rowspan ,column-headers-depth :colspan ,row-headers-depth)>)
             (render-column-headers ()
               (iter (for level-headers :initially column-headers :then (mappend #'children-of level-headers))
                     (while level-headers)
                     <tr ,(when (first-iteration-p)
                                (render-top-left-header))
                         ,(map nil (lambda (header)
                                     (bind ((expanded (not (find-ancestor (parent-component-of header) #'parent-component-of
                                                                          (lambda (parent)
                                                                            (not (expanded-p parent)))))))
                                       <td (:class "header" :colspan ,(count-leaves header))
                                           ,(if expanded
                                                (render-expanded-command header))
                                           ,(if expanded
                                                (render header)) >))
                               level-headers)>))
             (render-row-header (row-path)
               (iter (for (header . rest) :on row-path)
                     (for expanded :initially #t :then (and expanded (expanded-p header)))
                     (when (every (lambda (h)
                                    (eq h (first (children-of (parent-component-of h)))))
                                  rest)
                       <td (:class "header" :rowspan ,(count-leaves header))
                           ,(if expanded
                                (render-expanded-command header))
                           ,(if expanded
                                (render header)) >)))
             (render-rows ()
               (map-headers-pathes (lambda (row-path)
                                     <tr ,(render-row-header row-path)
                                         ,(render-cells row-path)>)
                                   row-headers))
             (render-cells (row-path)
               (map-headers-pathes (lambda (column-path)
                                     (render-cell row-path column-path))
                                   column-headers))
             (render-cell (row-path column-path)
               (bind ((expanded (and (every #'expanded-p row-path)
                                     (every #'expanded-p column-path)))
                      (cell (elt cells (cell-index row-path column-path)))
                      (empty? (typep cell 'empty-component)))
                 <td (:class ,(if (and (not empty?) (not expanded))
                                  "hidden data"
                                  "data"))
                     ,(when expanded
                            (render cell)) >)))
      <table (:class "extended-table")
        <tbody ,(render-column-headers)
               ,(render-rows)>>)))

;;;;;;
;;; Column header

(def component table-header-component (content-component)
  ((children nil :type components)
   (index nil :type integer)))

(def (macro e) table-header-component (content &body children)
  `(make-instance 'table-header-component
                  :content ,content
                  :children (list ,@children)))

(def function count-leaves (headers)
  (reduce #'+ (ensure-list headers)
          :key (lambda (header)
                 (aif (children-of header)
                      (reduce #'+ it :key #'count-leaves)
                      1))))

(def function collect-leaves (headers)
  (reduce #'append (ensure-list headers)
          :key (lambda (header)
                 (aif (children-of header)
                      (reduce #'append it :key #'collect-leaves)
                      (list header)))))

(def function count-depth (headers)
  (prog1-bind depth 0
    (labels ((traverse (header level)
               (aif (children-of header)
                    (dolist (child it)
                      (traverse child (1+ level)))
                    (setf depth (max level depth)))))
      (dolist (header (ensure-list headers))
        (traverse header 1)))))
