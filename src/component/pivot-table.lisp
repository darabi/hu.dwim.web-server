;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Pivot table

(def component pivot-table-component ()
  ((row-axes nil :type components)
   (column-axes nil :type components)
   (cells nil :type components)))

(def render pivot-table-component ()
  (bind (((:read-only-slots column-axes row-axes) -self-))
    (labels ((descartes-count (axes)
               (reduce #'* axes :key [length (categories-of !1)]))
             (render-cell (row-path column-path)
               ;; TODO: get data
               <td ,(princ-to-string (random 10)) >)
             (render-row-axis (row-path) ;; e.g. row-path = '("Budapesti" "Céges"), row-axes = '(("Budapesti" "Vidéki") ("Magán" "Céges"))
               <tr ,@(iter (for (category . rest) :on row-path)
                           (for axes :on row-axes)
                           (when (every (lambda (c a)
                                          (eq c (first (categories-of a))))
                                        rest (cdr axes))
                             <td (:rowspan ,(descartes-count (cdr axes)))
                                 ,(render category)>))
                   <td>
                   ,@(traverse nil column-axes (lambda (column-path)
                                                 (render-cell row-path column-path)))>)
             (traverse (path remaining visitor)
               (if remaining
                   (bind ((categories (categories-of (car remaining))))
                     (dolist (category categories)
                       (traverse (cons category path) (cdr remaining) visitor)))
                   (funcall visitor (reverse path)))))
      <table (:border "1")
        <tbody
         ,@(iter (with total-count = (descartes-count column-axes))
                 (for count :initially 1 :then (* count (length (categories-of column-axis))))
                 (for column-axis :in column-axes)
                 <tr ,(when (first-iteration-p)
                            <td (:colspan ,(length row-axes) :rowspan ,(length column-axes))>)
                     <td (:style "width: 5px;" :title "TODO: Tooltip")>
                     ,@(iter (repeat count)
                             (appending (mapcar (lambda (category)
                                                  <td (:colspan ,(/ total-count count (length (categories-of column-axis))))
                                                      ,(render category)>)
                                                (categories-of column-axis)))) >)
         <tr ,@(iter (repeat (length row-axes))
                     (collect <td (:style "height: 5px;" :title "TODO: Tooltip")>))
             <td>
             <td (:colspan ,(descartes-count column-axes))>>
         ,@(traverse nil row-axes #'render-row-axis)>>)))

;;;;;;
;;; Pivot table category

(def component pivot-table-axis-component ()
  ((categories)))

;; TODO: move or kill?
(def function make-test-pivot-table ()
  (make-instance 'pivot-table-component
                 :row-axes (list (make-instance 'pivot-table-axis-component
                                                :categories (list (label "Budapesti")
                                                                  (label "Vidéki")))
                                 (make-instance 'pivot-table-axis-component
                                                :categories (list (label "Magán")
                                                                  (label "Céges"))))
                 :column-axes (list (make-instance 'pivot-table-axis-component
                                                   :categories (list (label "2007")
                                                                     (label "2008")))
                                    (make-instance 'pivot-table-axis-component
                                                   :categories (list (label "Sima")
                                                                     (label "Kézi"))))))
