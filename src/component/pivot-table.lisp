;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Pivot table

(def component pivot-table-component ()
  ((row-axes nil :type components)
   (column-axes nil :type components)
   (show-row-total #t :type boolean)
   (show-column-total #t :type boolean)
   (cells nil :type components)
   (row-total-cells nil :type components)
   (column-total-cells nil :type components)
   (grand-total-cell nil :type component)))

(def constructor pivot-table-component ()
  (setf-cell-indexing-multipliers -self-))

(def function setf-cell-indexing-multipliers (self)
  (bind ((multiplier 1))
    (iter (for column-axis :in (reverse (column-axes-of self)))
          (setf (cell-indexing-multiplier-of column-axis) multiplier)
          (setf multiplier (* multiplier (length (categories-of column-axis)))))
    (iter (for row-axis :in (reverse (row-axes-of self)))
          (setf (cell-indexing-multiplier-of row-axis) multiplier)
          (setf multiplier (* multiplier (length (categories-of row-axis)))))))

(def function count-axes-cartesian-product (axes)
  (reduce #'* axes :key [length (categories-of !1)]))

(def render pivot-table-component ()
  (bind (((:read-only-slots column-axes row-axes show-row-total show-column-total cells row-total-cells column-total-cells grand-total-cell) -self-))
    (labels ((render-row-axis (row-path) ;; e.g. row-path = '("Budapesti" "Céges"), row-axes = '(("Budapesti" "Vidéki") ("Magán" "Céges"))
               <tr ,@(iter (for (category . rest) :on row-path)
                           (for axes :on row-axes)
                           (when (every (lambda (c a)
                                          (eq c (first (categories-of a))))
                                        rest (cdr axes))
                             <td (:class "axis" :rowspan ,(count-axes-cartesian-product (cdr axes)))
                                 ,(render category)>))
                   <td (:class "separator")>
                   ,@(when cells
                           (traverse nil column-axes
                                     (lambda (column-path)
                                       <td (:class "data")
                                           ,(render (elt cells (pivot-table-cell-index -self- row-path column-path)))>)))
                   ,(when (and show-row-total
                               row-total-cells)
                          <td (:class "total data")
                              ,(render (elt row-total-cells (/ (pivot-table-path-cell-index row-axes row-path)
                                                               (cell-indexing-multiplier-of (last-elt row-axes)))))>)>)
             (traverse (path remaining visitor)
               (if remaining
                   (bind ((categories (categories-of (car remaining))))
                     (dolist (category categories)
                       (traverse (cons category path) (cdr remaining) visitor)))
                   (list (funcall visitor (reverse path))))))
      <table (:class "pivot")
        <tbody
         ,@(iter (with total-count = (count-axes-cartesian-product column-axes))
                 (for count :initially 1 :then (* count (length (categories-of column-axis))))
                 (for column-axis :in column-axes)
                 (for i :from 0)
                 (bind ((i i)) ;; TODO: fuck iterate
                   <tr ,(when (first-iteration-p)
                              <td (:class "header" :colspan ,(max 1 (length row-axes)) :rowspan ,(max 1 (length column-axes)))
                                  "Pivot">)
                       <td (:class "column-tooltip" :title "TODO: Tooltip")
                           ,(render (command (icon rotate-counter-clockwise)
                                             (make-action
                                               (bind ((axis (elt column-axes i)))
                                                 (setf (row-axes-of -self-) (cons axis row-axes))
                                                 (setf (column-axes-of -self-) (remove axis column-axes))
                                                 (setf-cell-indexing-multipliers -self-)
                                                 (setf (component-value-of -self-) (component-value-of -self-))))))>
                       ,@(iter (repeat count)
                               (appending (mapcar (lambda (category)
                                                    <td (:class "axis" :colspan ,(/ total-count count (length (categories-of column-axis))))
                                                        ,(render category)>)
                                                  (categories-of column-axis))))
                       ,(when (first-iteration-p)
                              <td (:class "total" :rowspan ,(length column-axes))
                                  "Összesen">)>))
         <tr ,@(iter (for i :from 0 :below (length row-axes))
                     (bind ((i i)) ;; TODO: fuck iterate
                       (collect <td (:class "row-tooltip" :title "TODO: Tooltip")
                                    ,(render (command (icon rotate-clockwise)
                                                      (make-action
                                                        (bind ((axis (elt row-axes i)))
                                                          (setf (row-axes-of -self-) (remove axis row-axes))
                                                          (setf (column-axes-of -self-) (cons axis column-axes))
                                                          (setf-cell-indexing-multipliers -self-)
                                                          (setf (component-value-of -self-) (component-value-of -self-))))))>)))
             <td (:class "separator")>
             <td (:class "separator" :colspan ,(count-axes-cartesian-product column-axes))>
             ,(when show-row-total
                    <td (:class "separator")>) >
         ,@(traverse nil row-axes #'render-row-axis)
         ,(when show-column-total
                <tr <td (:class "total" :colspan ,(max 1 (length row-axes)))
                        "Összesen">
                    <td (:class "separator")>
                    ,@(when column-total-cells
                            (traverse nil column-axes
                                      (lambda (column-path)
                                        <td (:class "total data")
                                            ,(render (elt column-total-cells (pivot-table-path-cell-index column-axes column-path)))>)))
                    ,(when show-row-total
                           <td (:class "grand total data")
                               ,(render grand-total-cell)>)>)>>)))

(def function pivot-table-cell-index (pivot-table row-path column-path)
  (+ (pivot-table-path-cell-index (row-axes-of pivot-table) row-path)
     (pivot-table-path-cell-index (column-axes-of pivot-table) column-path)))

(def function pivot-table-path-cell-index (axes path)
  (iter (with index = 0)
        (for axis :in axes)
        (for category :in path)
        (for category-index = (position category (categories-of axis)))
        (incf index (* category-index (cell-indexing-multiplier-of axis)))
        (finally (return index))))

(def icon rotate-clockwise "static/wui/icons/20x20/clockwise-arrow.png" :label nil)
(defresources hu
  (icon-tooltip.rotate-clockwise "Elforgatás a másik tengelyre"))
(defresources en
  (icon-tooltip.rotate-clockwise "Rotate to other axis"))

(def icon rotate-counter-clockwise "static/wui/icons/20x20/counter-clockwise-arrow.png" :label nil)
(defresources hu
  (icon-tooltip.rotate-clockwise "Elforgatás a másik tengelyre"))
(defresources en
  (icon-tooltip.rotate-clockwise "Rotate to other axis"))

;;;;;;
;;; Pivot table axis

(def component pivot-table-axis-component ()
  ((categories nil :type component)
   (cell-indexing-multiplier :type integer)))

;;;;;
;;; Pivot table category

(def component pivot-table-category-component (content-component)
  ())
