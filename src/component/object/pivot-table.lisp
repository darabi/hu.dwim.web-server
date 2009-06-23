;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list pivot table

(def (component e) standard-object-list-pivot-table-component (standard-object-list/mixin pivot-table-component)
  ())

(def refresh-component standard-object-list-pivot-table-component
  (bind (((:read-only-slots row-axes column-axes column-leaf-count) -self-)
         ((:slots instances cells) -self-))
    (setf cells
          (bind ((result (iter (repeat column-leaf-count)
                               (collect (empty)))))
            (flet ((map-product* (function &rest lists)
                     (when lists
                       (apply #'map-product function (car lists) (cdr lists)))))
              (apply #'map-product*
                     (lambda (&rest row-path)
                       (push (empty) result)
                       (apply #'map-product*
                              (lambda (&rest column-path)
                                (push (make-standard-object-list-pivot-table-cell
                                       row-path
                                       column-path
                                       (filter-if (lambda (instance)
                                                    (and (matches-axis-path instance row-path)
                                                         (matches-axis-path instance column-path)))
                                                  instances))
                                      result))
                              (mapcar #'categories-of column-axes)))
                     (mapcar #'categories-of row-axes)))
            (nreverse result)))))

(def function matches-axis-path (instance path)
  (every (lambda (category)
           (funcall (predicate-of category) instance))
         path))

(def function make-standard-object-list-pivot-table-cell (row-path column-path instances)
  ;; TODO: what if not all instances have the same class? how do we find the base class?
  (if instances
      (bind ((class (class-of (first instances)))
             (aggregator-category
              (or (find-if (of-type 'standard-object-list-pivot-table-aggregator-category-component) row-path)
                  (find-if (of-type 'standard-object-list-pivot-table-aggregator-category-component) column-path))))
        (if aggregator-category
            (bind ((aggregated-slots (collect-standard-object-list-aggregator-slots (class-prototype (find-class 'standard-object-list-aggregator)) class))
                   (slot-name (slot-definition-name (first aggregated-slots))))
              (assert (length= 1 aggregated-slots))
              ;; TODO: make it a command which expands to the standard object list table (maybe aggregator list?)
              (funcall (aggregator-of aggregator-category) instances
                       (lambda (instance)
                         (slot-value instance slot-name))))
            (make-instance 'standard-object-list-inspector :instances instances)))
      (empty)))

;;;;;;
;;; Standard object list pivot table category

(def (component e) standard-object-list-pivot-table-category-component (pivot-table-category-component)
  ((predicate :type function)))

(def (function e) make-any-instance-standard-object-list-pivot-table-category ()
  (make-instance 'standard-object-list-pivot-table-category-component
                 :content "Mind"
                 :predicate (constantly #t)))

;;;;;;
;;; Standard object list pivot table aggregator category

(def (component e) standard-object-list-pivot-table-aggregator-category-component (standard-object-list-pivot-table-category-component)
  ((aggregator :type function)))

(def (function e) make-aggregator-pivot-table-axis ()
  (make-instance 'pivot-table-axis-component
                 :categories (list (make-instance 'standard-object-list-pivot-table-aggregator-category-component
                                                  :content "Darab"
                                                  :predicate (constantly #t)
                                                  :aggregator (lambda (instances value-thunk)
                                                                (declare (ignore value-thunk))
                                                                (length instances)))
                                   (make-instance 'standard-object-list-pivot-table-aggregator-category-component
                                                  :content "Minimum"
                                                  :predicate (constantly #t)
                                                  :aggregator (lambda (instances value-thunk)
                                                                (iter (for instance :in instances)
                                                                      (minimizing (funcall value-thunk instance)))))
                                   (make-instance 'standard-object-list-pivot-table-aggregator-category-component
                                                  :content "Maximum"
                                                  :predicate (constantly #t)
                                                  :aggregator (lambda (instances value-thunk)
                                                                (iter (for instance :in instances)
                                                                      (maximizing (funcall value-thunk instance)))))
                                   (make-instance 'standard-object-list-pivot-table-aggregator-category-component
                                                  :content "Átlag"
                                                  :predicate (constantly #t)
                                                  :aggregator (lambda (instances value-thunk)
                                                                ;; TODO: really coerce here
                                                                (coerce (/ (reduce #'+ instances :key value-thunk)
                                                                           (length instances))
                                                                        'float)))
                                   (make-instance 'standard-object-list-pivot-table-aggregator-category-component
                                                  :content "Összesen"
                                                  :predicate (constantly #t)
                                                  :aggregator (lambda (instances value-thunk)
                                                                (reduce #'+ instances :key value-thunk))))))