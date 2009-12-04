;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customizations

#+nil
(def layered-method collect-standard-object-detail-filter-slots ((component standard-object-detail-filter) (class hu.dwim.meta-model::entity) (prototype hu.dwim.perec::persistent-object))
  (filter-if (lambda (slot)
               (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::filter-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

#+nil
(def layered-method collect-standard-object-detail-filter-slots ((component standard-object-detail-filter) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object))
  (remove-if #'hu.dwim.perec:persistent-object-internal-slot-p (call-next-method)))

(def layered-method execute-filter ((component t/filter) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) value)
  (hu.dwim.perec::with-transaction
    (hu.dwim.perec::execute-query (make-filter-query component class prototype value))))

(def method predicate-function ((component timestamp/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'equal)))
  'local-time:timestamp=)

(def method predicate-function ((component timestamp/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'less-than)))
  'local-time:timestamp<)

(def method predicate-function ((component timestamp/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'less-than-or-equal)))
  'local-time:timestamp<=)

(def method predicate-function ((component timestamp/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'greater-than)))
  'local-time:timestamp>)

(def method predicate-function ((component timestamp/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'greater-than-or-equal)))
  'local-time:timestamp>=)

(def method predicate-function ((component time/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'equal)))
  'local-time:timestamp=)

(def method predicate-function ((component time/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'less-than)))
  'local-time:timestamp<)

(def method predicate-function ((component time/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'less-than-or-equal)))
  'local-time:timestamp<=)

(def method predicate-function ((component time/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'greater-than)))
  'local-time:timestamp>)

(def method predicate-function ((component time/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'greater-than-or-equal)))
  'local-time:timestamp>=)

(def method predicate-function ((component date/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'equal)))
  'local-time:timestamp=)

(def method predicate-function ((component date/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'less-than)))
  'local-time:timestamp<)

(def method predicate-function ((component date/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'less-than-or-equal)))
  'local-time:timestamp<=)

(def method predicate-function ((component date/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'greater-than)))
  'local-time:timestamp>)

(def method predicate-function ((component date/filter) (class hu.dwim.perec::persistent-class) (predicate (eql 'greater-than-or-equal)))
  'local-time:timestamp>=)

;;;;;;
;;; Query builder

(def class* filter-query ()
  ((query nil)
   (query-variable-stack nil)))

(def (with-macro* e) with-new-query-variable (variable-name filter-query class-name)
  (bind ((query (query-of filter-query))
         (query-variable (hu.dwim.perec::add-query-variable query (gensym (symbol-name class-name)))))
    (push query-variable (query-variable-stack-of filter-query))
    ;; KLUDGE: was `(typep ,query-variable ',class-name) but with-macro didn't like it
    (hu.dwim.perec::add-assert query (list 'typep query-variable (list 'quote class-name)))
    (multiple-value-prog1
        (-body- (query-variable variable-name))
      (pop (query-variable-stack-of filter-query)))))

(def (layered-function e) make-filter-query (component class prototype value)
  (:method ((component t/filter) class prototype value)
    (bind ((query (hu.dwim.perec::make-instance 'hu.dwim.perec::query))
           (filter-query (make-instance 'filter-query :query query)))
      (with-new-query-variable (query-variable filter-query (class-name class))
        (hu.dwim.perec::add-collect query query-variable)
        (make-filter-query* component filter-query))
      query)))

(def (layered-function e) make-filter-query* (component filter-query)
  (:method ((component content/mixin) filter-query)
    (make-filter-query* (content-of component) filter-query))

  (:method ((component contents/mixin) filter-query)
    (foreach (lambda (content)
               (make-filter-query* content filter-query))
             (contents-of component)))

  (:method ((component place-group-list/name-value-list/filter) filter-query)
    (foreach (lambda (slot-value-group)
               (make-filter-query* slot-value-group filter-query))
             (contents-of component)))

  (:method ((component place/name-value-pair/filter) filter-query)
    (bind ((place-filter (value-of component))
           (value-filter (content-of place-filter))
           (place (component-value-of component)))
      (etypecase place
        (object-slot-place
         (bind ((slot (slot-of place)))
           (when (typep slot 'hu.dwim.perec::persistent-effective-slot-definition)
             (cond ((or (typep value-filter 'primitive/filter)
                        (typep value-filter 't/inspector))
                    ;; TODO: use when, not unless
                    (when (use-in-filter? component)
                      (bind ((value (component-value-of value-filter))
                             (ponated-predicate (make-filter-query-predicate (content-of place-filter) (class-of (instance-of place))
                                                                             (selected-predicate-of component) slot filter-query value)))
                        (hu.dwim.perec::add-assert (query-of filter-query)
                                                   (if (negated? component)
                                                       `(not ,ponated-predicate)
                                                       ponated-predicate)))))
                   ((and (typep value-filter 't/filter)
                         (not (typep (content-of value-filter) 't/reference/filter)))
                    (with-new-query-variable (query-variable filter-query (class-name (component-value-of value-filter)))
                      (hu.dwim.perec::add-assert (query-of filter-query)
                                                 `(eq ,query-variable
                                                      (,(hu.dwim.perec::reader-name-of slot)
                                                        ,(second (query-variable-stack-of filter-query)))))
                      (make-filter-query* value-filter filter-query)))))))))))

(def generic make-filter-query-predicate (component class predicate slot query value)
  (:method (component class predicate slot query value)
    `(,(predicate-function component class predicate)
       (,(hu.dwim.perec::reader-name-of slot) ,(first (query-variable-stack-of query)))
       (quote ,value)))

  (:method (component class (predicate (eql 'like)) slot query value)
    ;; TODO due to a postgres bug it doesn't work if there's an accented letter in the regexp with a non-matching upper/lower case
    ;; http://wiki.postgresql.org/wiki/Todo
    ;; "Fix ILIKE and regular expressions to handle case insensitivity properly in multibyte encodings"
    #+nil
    `(hu.dwim.perec:re-like
      (,(hu.dwim.perec::reader-name-of slot) ,(first (query-variable-stack-of query)))
      (quote ,value)
      :case-sensitive-p #f)

    `(hu.dwim.perec:like
      (,(hu.dwim.perec::reader-name-of slot) ,(first (query-variable-stack-of query)))
      ,(string+ "%" value "%")
      :case-sensitive-p #f)))
