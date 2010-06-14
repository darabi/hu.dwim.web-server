;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/alternator/filter

(def (component e) t/alternator/filter (t/filter
                                        t/alternator/presentation
                                        cloneable/component
                                        layer/mixin
                                        title/mixin
                                        component-result/mixin)
  ()
  (:documentation "Generic factory configuration (all components are available):

(T/ALTERNATOR/FILTER                              ; filter for something (alternator)
 (T/PLACE-LIST/FILTER                             ; filter for a list of places of something
  (PLACE-LIST/ALTERNATOR/FILTER                   ; filter for a list of places (alternator)
   (PLACE-LIST/PLACE-GROUP-LIST/FILTER            ; filter for a grouping of a list of places
    (PLACE-GROUP-LIST/ALTERNATOR/FILTER           ; filter for a list of place groups (alternator)
     (PLACE-GROUP-LIST/NAME-VALUE-LIST/FILTER     ; filter for a list of place groups, display as a name value list
      (PLACE-GROUP/ALTERNATOR/FILTER              ; filter for a group of places (alternator)
       (PLACE-GROUP/NAME-VALUE-GROUP/FILTER       ; filter for a group of places, display as a name value group
        (PLACE/ALTERNATOR/FILTER                  ; filter for a place (alternator)
         (PLACE/NAME-VALUE-PAIR/FILTER            ; filter for a place, display as a name value pair
          (PLACE/NAME/FILTER                      ; filter the name of a place
           (STRING/ALTERNATOR/FILTER              ; filter a string (alternator)
            (STRING/TEXT/FILTER                   ; filter a string, display as text
             STRING)))                            ; immediate
          (PLACE/VALUE/FILTER                     ; filter for the value of a place
           (T/ALTERNATOR/FILTER))))               ; filter for something (alternator)
        ...))
      ...))))))

Optimized factory configuration (default):

(T/ALTERNATOR/FILTER                              ; filter for something (alternator)
 (PLACE-GROUP-LIST/NAME-VALUE-LIST/FILTER         ; filter for a list of place groups, display as a name value list
  (PLACE-GROUP/NAME-VALUE-GROUP/FILTER            ; filter for a group of places, display as a name value group
   (PLACE/NAME-VALUE-PAIR/FILTER                  ; filter for a place, display as a name value pair
    STRING                                        ; immediate
    (PLACE/VALUE/FILTER                           ; filter for the value of a place
     (T/ALTERNATOR/FILTER)))                      ; filter for something (alternator)
   ...)
  ...))
"))

(def subtype-mapper *filter-type-mapping* t t/alternator/filter)

(def render-component t/alternator/filter
  (render-title-for -self-)
  (render-alternator-interior -self-)
  (render-result-for -self-))

(def render-xhtml t/alternator/filter
  (with-render-xhtml-alternator -self-
    (call-next-layered-method)))

(def layered-method make-alternatives ((component t/alternator/filter) class prototype value)
  (list (make-instance 't/name-value-list/filter
                       :component-value value
                       :component-value-type (component-value-type-of component))
        (make-instance 't/reference/filter
                       :component-value value
                       :component-value-type (component-value-type-of component))))

(def layered-method make-command-bar-commands ((component t/alternator/filter) class prototype value)
  (optional-list* (make-execute-filter-command component class prototype value) (call-next-layered-method)))

(def method collect-filter-predicates ((self t/alternator/filter))
  '(equal))

(def (icon e) execute-filter)

(def layered-method make-execute-filter-command ((component t/alternator/filter) class prototype value)
  (when (authorize-operation *application* `(make-execute-filter-command :class ,class))
    (make-replace-and-push-back-command (delay (result-of component))
                                        (delay (with-restored-component-environment component
                                                 (with-interaction component
                                                   (make-result component class prototype (execute-filter component class prototype value)))))
                                        (list :content (icon/widget execute-filter)
                                              :default #t
                                              :subject-component component)
                                        (list :content (icon/widget navigate-back)))))

(def layered-method make-result ((component t/alternator/filter) class prototype (value list))
  (make-inspector `(or null (cons ,(class-name (component-dispatch-class component)) t)) :value value))

(def layered-method make-result :around ((component t/alternator/filter) class prototype value)
  (prog1-bind component
      (call-next-layered-method)
    (if value
        (add-component-information-message component (matches-were-found (length value)))
        (add-component-warning-message component #"no-matches-were-found"))))

(def (layered-function e) execute-filter (component class prototype value)
  (:method ((component t/alternator/filter) class prototype value)
    (bind ((result nil)
           (predicate (make-filter-predicate component class prototype value)))
      (map-filter-input component class prototype value
                        (lambda (candidate)
                          (when (funcall predicate candidate)
                            (push candidate result))))
      result)))

(def (layered-function e) make-filter-predicate (component class prototype value)
  (:method ((component content/mixin) class prototype value)
    (make-filter-predicate (content-of component) class prototype value)))

(def (layered-function e) map-filter-input (component class prototype value function)
  (:method ((component t/alternator/filter) class prototype value function)
    (sb-ext::gc :full #t)
    (sb-vm::map-allocated-objects
     (lambda (candidate type size)
       (declare (ignore type size)
                (optimize (debug 0) (speed 3)))
       (funcall function candidate))
     :dynamic #t)))

;;;;;;
;;; t/reference/filter

(def (component e) t/reference/filter (t/filter t/reference/presentation)
  ())

(def layered-method make-reference-content ((component t/reference/filter) class prototype value)
  (localized-class-name (component-dispatch-class component)))

;;;;;;
;;; t/detail/filter

(def (component e) t/detail/filter (t/filter t/detail/presentation)
  ())

;;;;;;
;;; t/name-value-list/filter

(def (component e) t/name-value-list/filter (t/detail/filter t/name-value-list/presentation)
  ())

(def layered-method collect-presented-slots ((component t/name-value-list/filter) class prototype value)
  (class-slots (component-dispatch-class component)))

(def layered-method make-presented-place-group ((component t/name-value-list/filter) class prototype value)
  (make-place-group nil (mapcar [make-object-slot-place (class-prototype (component-dispatch-class component)) !1] value)))

(def layered-methods make-content-presentation
  (:method ((component t/name-value-list/filter) class prototype (value place-group))
    (make-instance 'place-group-list/name-value-list/filter
                   :component-value value
                   :component-value-type (component-value-type-of component)))

  (:method ((component t/name-value-list/filter) class prototype (value sequence))
    (make-instance 'sequence/list/filter
                   :component-value value
                   :component-value-type (component-value-type-of component)))

  (:method ((component t/name-value-list/filter) class prototype (value number))
    value)

  (:method ((component t/name-value-list/filter) class prototype (value string))
    value)

  (:method ((component t/name-value-list/filter) class prototype value)
    (make-instance 't/reference/filter
                   :component-value value
                   :component-value-type (component-value-type-of component)
                   :action nil :enabled #f)))

(def layered-method make-filter-predicate ((component t/name-value-list/filter) class prototype value)
  (bind ((predicate (call-next-layered-method)))
    (lambda (candidate)
      (and (typep candidate (component-value-type-of component))
           (funcall predicate candidate)))))
;;;;;;
;;; place-group-list/name-value-list/filter

(def (component e) place-group-list/name-value-list/filter (t/detail/filter place-group-list/name-value-list/presentation)
  ())

(def layered-method collect-presented-place-groups ((component place-group-list/name-value-list/filter) class prototype (value place-group))
  (list value))

(def layered-method make-content-presentation ((component place-group-list/name-value-list/filter) class prototype (value place-group))
  (make-instance 'place-group/name-value-group/filter
                 :component-value value
                 :component-value-type (component-value-type-of component)))

(def layered-method make-filter-predicate ((component contents/mixin) class prototype value)
  (bind ((predicates (iter (for content :in (contents-of component))
                           (awhen (make-filter-predicate content class prototype value)
                             (collect it)))))
    (lambda (candidate)
      (every (lambda (predicate)
               (funcall predicate candidate))
             predicates))))

;;;;;;
;;; place-group/name-value-group/filter

(def (component e) place-group/name-value-group/filter (t/detail/filter place-group/name-value-group/presentation)
  ())

(def layered-method make-content-presentation ((component place-group/name-value-group/filter) class prototype (value object-slot-place))
  (make-instance 'place/name-value-pair/filter
                 :component-value value
                 :component-value-type (component-value-type-of component)))

;;;;;;
;;; place/name-value-pair/filter

;; TODO: move these slots down one level?
(def (component e) place/name-value-pair/filter (t/detail/filter place/name-value-pair/presentation)
  ((use-in-filter #f :type boolean)
   (use-in-filter-id)
   (negated #f :type boolean)
   (selected-predicate nil :type symbol)))

(def layered-method make-value-presentation ((component place/name-value-pair/filter) class prototype value)
  (make-instance 'place/value/filter
                 :component-value value
                 :component-value-type (component-value-type-of component)))

(def layered-method make-filter-predicate ((component place/name-value-pair/filter) class prototype value)
  (bind ((place (component-value-of component))
         (predicate (predicate-function (content-of (value-of component)) prototype (selected-predicate-of component))))
    (when (use-in-filter? component)
      (lambda (candidate)
        ;; TODO: use place API instead of slot API
        (bind ((slot-name (slot-definition-name (slot-of place))))
          (when (slot-boundp candidate slot-name)
            (bind ((result (funcall predicate
                                    (slot-value candidate slot-name)
                                    (component-value-of (content-of (value-of component))))))
              (if (negated? component)
                  (not result)
                  result))))))))

;; TODO: do we need this parentism
(def method use-in-filter? ((self parent/mixin))
  (use-in-filter? (parent-component-of self)))

(def method use-in-filter? ((self component))
  #t)

;; TODO: do we need this parentism
(def method use-in-filter-id-of ((self parent/mixin))
  (use-in-filter-id-of (parent-component-of self)))

(def method use-in-filter-id-of ((self component))
  (generate-unique-component-id))

;; TODO: move this to a component?
(def render-component place/name-value-pair/filter
  (render-name-for -self-)
  (render-filter-predicate-for -self-)
  (render-use-in-filter-marker-for -self-)
  (render-value-for -self-))

;;;;;;
;;; Icon

(def (icon e) equal-predicate)

(def (icon e) like-predicate)

(def (icon e) less-than-predicate)

(def (icon e) less-than-or-equal-predicate)

(def (icon e) greater-than-predicate)

(def (icon e) greater-than-or-equal-predicate)

(def (icon e) negated-predicate)

(def (icon e) ponated-predicate)

;;;;;;
;;; Util

(def function localize-predicate (predicate)
  (lookup-resource (string+ "predicate." (symbol-name predicate))))

(def function predicate-icon-style-class (predicate)
  (string+ "icon predicate " (string-downcase predicate)))

(def generic predicate-function (component class predicate)
  (:method ((component t/filter) class predicate)
    'eq)

  (:method ((component primitive/filter) class (predicate null))
    'equal)

  (:method ((component primitive/filter) class (predicate (eql 'equal)))
    'equal)

  (:method ((component string/filter) class (predicate (eql 'like)))
    (bind ((scanner nil)
           (previous-regexp nil))
      (lambda (value regexp)
        (unless (equal regexp previous-regexp)
          (setf scanner (cl-ppcre:create-scanner regexp :case-insensitive-mode #t)))
        (and (or (symbolp value)
                 (stringp value))
             (cl-ppcre:do-matches (match-start match-end scanner (string value) nil)
               (return #t))))))

  (:method ((component string/filter) class (predicate (eql 'less-than)))
    'string<)

  (:method ((component string/filter) class (predicate (eql 'less-than-or-equal)))
    'string<=)

  (:method ((component string/filter) class (predicate (eql 'greater-than)))
    'string>)

  (:method ((component string/filter) class (predicate (eql 'greater-than-or-equal)))
    'string>=)

  (:method ((component number/filter) class (predicate (eql 'less-than)))
    '<)

  (:method ((component number/filter) class (predicate (eql 'less-than-or-equal)))
    '<=)

  (:method ((component number/filter) class (predicate (eql 'greater-than)))
    '>)

  (:method ((component number/filter) class (predicate (eql 'greater-than-or-equal)))
    '>=))

(def function render-filter-predicate-for (self)
  (bind (((:slots negated selected-predicate) self)
         ;; TODO: KLUDGE: don't look down this deep
         (filter-predicates (collect-filter-predicates (content-of (aprog1 (value-of self)
                                                                       (ensure-refreshed it))))))
    (if filter-predicates
        (progn
          (unless selected-predicate
            (setf selected-predicate (first filter-predicates)))
          <td ,(render-checkbox-field negated
                                      :value-sink (lambda (value) (setf negated value))
                                      :checked-class "icon predicate negated"
                                      :unchecked-class "icon predicate ponated")>
          <td ,(if (length= 1 filter-predicates)
                   <div (:class ,(predicate-icon-style-class (first filter-predicates)))>
                   (render-popup-menu-select-field (localize-predicate selected-predicate)
                                                   (mapcar #'localize-predicate filter-predicates)
                                                   :value-sink (lambda (value)
                                                                 (setf selected-predicate (find value filter-predicates :key #'localize-predicate :test #'string=)))
                                                   :classes (mapcar #'predicate-icon-style-class filter-predicates)))>)
        <td (:colspan 2)>)))

(def function render-use-in-filter-marker-for (self)
  (bind ((id (generate-unique-component-id)))
    (setf (use-in-filter-id-of self) id)
    <td ,(render-checkbox-field (use-in-filter? self)
                                :id id
                                :value-sink (lambda (value) (setf (use-in-filter? self) value))
                                :checked-class "icon predicate use"
                                :unchecked-class "icon predicate ignore")>))
