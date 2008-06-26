;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list pivot table

(def component standard-object-list-pivot-table-component (abstract-standard-object-list-component pivot-table-component)
  ())

(def method (setf component-value-of) :after (new-value (self standard-object-list-pivot-table-component))
  (bind (((:read-only-slots instances row-axes column-axes) self))
    ;; TODO: where should we revive?
    (setf (instances-of self) (mapcar #'prc::load-instance instances))
    (setf (cells-of self)
          (nreverse (prog1-bind cells nil
                      (apply #'map-product (lambda (&rest row-path)
                                             (apply #'map-product (lambda (&rest column-path)
                                                                    (push (make-instance 'standard-object-list-aggregator-component
                                                                                         :instances (filter-if (lambda (instance)
                                                                                                                 (and (matches-axis-path instance row-path)
                                                                                                                      (matches-axis-path instance column-path)))
                                                                                                               (instances-of self)))
                                                                          cells))
                                                    (mapcar #'categories-of column-axes)))
                             (mapcar #'categories-of row-axes)))))
    (setf (row-total-cells-of self)
          (apply #'map-product (lambda (&rest row-path)
                                 (make-instance 'standard-object-list-aggregator-component :instances (filter-if (lambda (instance)
                                                                                                                   (matches-axis-path instance row-path))
                                                                                                                 (instances-of self))))
                 (mapcar #'categories-of row-axes)))
    (setf (column-total-cells-of self)
          (apply #'map-product (lambda (&rest column-path)
                                 (make-instance 'standard-object-list-aggregator-component :instances (filter-if (lambda (instance)
                                                                                                                   (matches-axis-path instance column-path))
                                                                                                                 (instances-of self))))
                 (mapcar #'categories-of column-axes)))
    (setf (grand-total-cell-of self)
          (make-instance 'standard-object-list-aggregator-component :instances instances))))

(def function matches-axis-path (instance path)
  (every (lambda (category)
           (funcall (predicate-of category) instance))
         path))

;;;;;;
;;; Standard object list pivot table category

(def component standard-object-list-pivot-table-category-component (pivot-table-category-component)
  ((predicate :type function)))

;;;;;;
;;; Standard object list aggregator

;; TODO: how to use aggregate functions sum, maximum, minimum, average, variance, count, etc.?
;; TODO: move? and reuse?
(def component standard-object-list-aggregator-component (abstract-standard-object-list-component)
  ((expand-command :type component)))

(def constructor standard-object-list-aggregator-component ()
  (bind (((:read-only-slots instances) -self-))
    (setf (expand-command-of -self-) (make-replace-and-push-back-command -self- (delay (make-viewer-component instances))
                                                                         (list :icon (icon expand :label (length instances)))
                                                                         (list :icon (icon back))))))

(def render standard-object-list-aggregator-component ()
  (render (expand-command-of -self-)))
