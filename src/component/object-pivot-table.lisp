;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list pivot table

(def component standard-object-list-pivot-table-component (abstract-standard-object-list-component pivot-table-component)
  ())

;; TODO: fix alexandria
(def function map-product* (function &rest lists)
  (when lists
    (apply #'map-product function (car lists) (cdr lists))))

(def method (setf component-value-of) :after (new-value (self standard-object-list-pivot-table-component))
     (bind (((:read-only-slots row-axes column-axes) self)
            ((:slots instances) self))
       (setf instances (mapcar #'reuse-standard-object-instance instances))
       (setf (cells-of self)
             (nreverse (prog1-bind cells nil
                         (apply #'map-product* (lambda (&rest row-path)
                                                 (apply #'map-product* (lambda (&rest column-path)
                                                                         (push (make-standard-object-list-pivot-table-cell (filter-if (lambda (instance)
                                                                                                                                        (and (matches-axis-path instance row-path)
                                                                                                                                             (matches-axis-path instance column-path)))
                                                                                                                                      (instances-of self)))
                                                                               cells))
                                                        (mapcar #'categories-of column-axes)))
                                (mapcar #'categories-of row-axes)))))
       (setf (row-total-cells-of self)
             (apply #'map-product* (lambda (&rest row-path)
                                     (make-standard-object-list-pivot-table-cell (filter-if (lambda (instance)
                                                                                              (matches-axis-path instance row-path))
                                                                                            (instances-of self))))
                    (mapcar #'categories-of row-axes)))
       (setf (column-total-cells-of self)
             (apply #'map-product* (lambda (&rest column-path)
                                     (make-standard-object-list-pivot-table-cell (filter-if (lambda (instance)
                                                                                              (matches-axis-path instance column-path))
                                                                                            (instances-of self))))
                    (mapcar #'categories-of column-axes)))
       (setf (grand-total-cell-of self)
             (make-standard-object-list-pivot-table-cell instances))))

(def function matches-axis-path (instance path)
  (every (lambda (category)
           (funcall (predicate-of category) instance))
         path))

(def function make-standard-object-list-pivot-table-cell (instances)
  (if instances
      ;; TODO: what if not all instances have the same class? how do we find the base class?
      (make-instance 'standard-object-list-component
                     :instances instances
                     :alternatives-factory (lambda (class instances)
                                             (list* (delay-alternative-component-type 'standard-object-list-aggregator-component :instances instances :the-class (class-of (first instances)))
                                                    (make-standard-object-list-alternatives class instances))))
      (make-instance 'empty-component)))

;;;;;;
;;; Standard object list pivot table category

(def component standard-object-list-pivot-table-category-component (pivot-table-category-component)
  ((predicate :type function)))
