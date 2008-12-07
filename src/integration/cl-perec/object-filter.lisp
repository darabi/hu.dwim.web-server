;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customizations

(def layered-method make-standard-object-detail-filter-class ((component standard-object-detail-filter) (class prc::persistent-class) (prototype prc::persistent-object))
  (if (dmm::developer-p (dmm::current-effective-subject))
      (make-viewer class :default-component-type 'reference-component)
      (call-next-method)))

(def layered-method collect-standard-object-detail-filter-slots ((component standard-object-detail-filter) (class dmm::entity) (prototype prc::persistent-object))
  (filter-if (lambda (slot)
               (dmm::authorize-operation 'dmm::filter-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

(def layered-method collect-standard-object-detail-filter-slots ((component standard-object-detail-filter) (class prc::persistent-class) (prototype prc::persistent-object))
  (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

(def layered-method execute-filter-instances ((component standard-object-filter) (class prc::persistent-class))
  (prc::execute-query (build-filter-query component)))

(def method predicate-function ((component timestamp-component) (class prc::persistent-class) (predicate (eql '=)))
  'local-time:timestamp=)

(def method predicate-function ((component timestamp-component) (class prc::persistent-class) (predicate (eql '<)))
  'local-time:timestamp<)

(def method predicate-function ((component timestamp-component) (class prc::persistent-class) (predicate (eql '≤)))
  'local-time:timestamp<=)

(def method predicate-function ((component timestamp-component) (class prc::persistent-class) (predicate (eql '>)))
  'local-time:timestamp>)

(def method predicate-function ((component timestamp-component) (class prc::persistent-class) (predicate (eql '≥)))
  'local-time:timestamp>=)

;;;;;
;;; Query builder

(def class* filter-query ()
  ((query nil)
   (query-variable-stack nil)))

(def with-macro* with-new-query-variable (variable-name filter-query component)
  (bind ((query (query-of filter-query))
         (query-variable (prc::add-query-variable query (gensym (symbol-name (class-name (the-class-of component)))))))
    (push query-variable (query-variable-stack-of filter-query))
    (prc::add-assert query `(typep ,query-variable ',(class-name (the-class-of component))))
    (multiple-value-prog1
        (-body- (query-variable variable-name))
      (pop (query-variable-stack-of filter-query)))))

(def generic build-filter-query (component)
  (:method ((component standard-object-filter))
    (bind ((query (prc::make-instance 'prc::query))
           (filter-query (make-instance 'filter-query :query query)))
      (with-new-query-variable (query-variable filter-query component)
        (prc::add-collect query query-variable)
        (build-filter-query* component filter-query))
      query)))

(def generic build-filter-query* (component filter-query)
  (:method ((component standard-object-filter) filter-query)
    (build-filter-query* (content-of component) filter-query))

  (:method ((component standard-object-detail-filter) filter-query)
    (when-bind class-selector (class-selector-of component)
      (when-bind selected-class (component-value-of class-selector)
        (prc::add-assert (query-of filter-query) `(typep ,(first (query-variable-stack-of filter-query)) ,selected-class))))
    (foreach (lambda (slot-value-group)
               (build-filter-query* slot-value-group filter-query)) (slot-value-groups-of component)))

  (:method ((component standard-object-filter-reference) filter-query)
    (values))

  (:method ((component standard-object-slot-value-group-filter) filter-query)
    (dolist (slot-value (slot-values-of component))
      (build-filter-query* slot-value filter-query)))

  (:method ((component standard-object-slot-value-filter) filter-query)
    (bind ((place-filter (value-of component))
           (value-component (content-of place-filter))
           (slot (slot-of component)))
      (when (typep slot 'prc::persistent-effective-slot-definition)
        (cond ((or (typep value-component 'primitive-component)
                   (typep value-component 'standard-object-inspector))
               (when (use-in-filter-p place-filter)
                 (bind ((value (component-value-of value-component))
                        (ponated-predicate `(,(predicate-function place-filter (the-class-of component) (selected-predicate-of place-filter))
                                              (,(prc::reader-name-of slot)
                                                ,(first (query-variable-stack-of filter-query)))
                                              (quote ,value))))
                   (prc::add-assert (query-of filter-query)
                                    (if (negated-p place-filter)
                                        `(not ,ponated-predicate)
                                        ponated-predicate)))))
              ((and (typep value-component 'standard-object-filter)
                    (not (typep (content-of value-component) 'standard-object-filter-reference)))
               (with-new-query-variable (query-variable filter-query value-component)
                 (prc::add-assert (query-of filter-query)
                                  `(eq ,query-variable
                                       (,(prc::reader-name-of slot)
                                         ,(second (query-variable-stack-of filter-query)))))
                 (build-filter-query* value-component filter-query))))))))

(def method predicate-function ((component string-component) (class prc::persistent-class) (predicate (eql '~)))
  'prc::re-like)
