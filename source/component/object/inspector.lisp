;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/inspector

(def (component e) t/inspector (inspector/basic
                                t/presentation
                                cloneable/abstract
                                deep-arguments/mixin
                                layer/mixin)
  ()
  (:documentation "Generic factory version (all components are available):

(T/INSPECTOR                                      ; inspect something (alternator)
 (T/PLACE-LIST/INSPECTOR                          ; inspect a list of places of something
  (PLACE-LIST/INSPECTOR                           ; inspect a list of places (alternator)
   (PLACE-LIST/PLACE-GROUP-LIST/INSPECTOR         ; inspect a grouping of a list of places
    (PLACE-GROUP-LIST/INSPECTOR                   ; inspect a list of place groups (alternator)
     (PLACE-GROUP-LIST/NAME-VALUE-LIST/INSPECTOR  ; inspect a list of place groups, display as a name value list
      (PLACE-GROUP/INSPECTOR                      ; inspect a group of places (alternator)
       (PLACE-GROUP/NAME-VALUE-GROUP/INSPECTOR    ; inspect a group of places, display as a name value group
        (PLACE/INSPECTOR                          ; inspect a place (alternator)
         (PLACE/NAME-VALUE-PAIR/INSPECTOR         ; inspect a place, display as a name value pair
          (PLACE/NAME/INSPECTOR                   ; inspect the name of a place
           (STRING/INSPECTOR                      ; inspect a string (alternator)
            (STRING/TEXT/INSPECTOR                ; inspect a string, display as text
             STRING)))                            ; immediate
          (PLACE/VALUE/INSPECTOR                  ; inspect the value of a place
           (T/INSPECTOR))))                       ; inspect something (alternator)
        ...))
      ...))))))

Optimized factory configuration (default):

(T/INSPECTOR                                      ; inspect something (alternator)
 (PLACE-GROUP-LIST/NAME-VALUE-LIST/INSPECTOR      ; inspect a list of place groups, display as a name value list
  (PLACE-GROUP/NAME-VALUE-GROUP/INSPECTOR         ; inspect a group of places, display as a name value group
   (PLACE/NAME-VALUE-PAIR/INSPECTOR               ; inspect a place, display as a name value pair
    STRING                                        ; immediate
    (PLACE/VALUE/INSPECTOR                        ; inspect the value of a place
     (T/INSPECTOR)))                              ; inspect something (alternator)
   ...)
  ...))

(STRING/INSPECTOR                                 ; inspect a string (alternator)
 (STRING/TEXT/INSPECTOR)                          ; inspect a string as text
 (STRING/CHARACTER-SEQUENCE/INSPECTOR)            ; inspect a string as a sequence of characters
 ...)
"))

(def subtype-mapper *inspector-type-mapping* t t/inspector)

(def layered-method make-alternatives ((component t/inspector) class prototype value)
  (bind (((:read-only-slots editable-component edited-component component-value-type) component))
    (list (apply #'make-instance
                 't/name-value-list/inspector
                 :component-value value
                 :component-value-type component-value-type
                 :edited edited-component
                 :editable editable-component
                 (alternative-deep-arguments component 't/name-value-list/inspector))
          (make-instance 't/reference/inspector
                         :component-value value
                         :component-value-type component-value-type
                         :edited edited-component
                         :editable editable-component))))

;;;;;;
;;; t/reference/inspector

(def (component e) t/reference/inspector (inspector/abstract t/reference/presentation)
  ())

;;;;;;
;;; t/detail/inspector

(def (component e) t/detail/inspector (inspector/abstract t/detail/presentation)
  ())

;;;;;
;;; t/documentation/inspector

(def (component e) t/documentation/inspector (inspector/style t/detail/inspector contents/mixin)
  ())

(def refresh-component t/documentation/inspector
  (bind (((:slots contents component-value) -self-)
         (documentation (make-documentation -self- (component-dispatch-class -self-) (component-dispatch-prototype -self-) component-value)))
    (setf contents (parse-documentation documentation))))

(def render-component t/documentation/inspector
  (render-contents-for -self-))

(def render-xhtml t/documentation/inspector
  (with-render-style/abstract (-self-)
    (render-contents-for -self-)))

(def (generic e) make-documentation (component class prototype value)
  (:method :around (component class prototype value)
    (or (call-next-method) "No documentation"))

  (:method ((component t/documentation/inspector) class prototype value)
    (documentation value t))

  (:method ((component t/documentation/inspector) (class standard-class) (prototype built-in-class) (value built-in-class))
    (documentation value t))

  (:method ((component t/documentation/inspector) (class standard-class) (prototype class) (value class))
    (documentation value t))

  (:method ((component t/documentation/inspector) (class standard-class) (prototype standard-object) (value standard-object))
    (documentation class t)))

(def function parse-documentation (text)
  (iter (for part :in (split-sequence-by-partitioning text
                                                      (lambda (character)
                                                        (or (upper-case-p character)
                                                            (member character '(#\- #\/ #\. #\: #\'))))
                                                      (constantly #t)))
        (for targets = (when (> (length part) 2)
                         (deserialize/human-readable part)))
        (collect (cond ((null targets)
                        part)
                       ((and (typep targets 'sequence)
                             (length= 1 targets))
                        (make-value-inspector (first targets) :initial-alternative-type 't/reference/inspector))
                       (t
                        (make-value-inspector targets :initial-alternative-type 't/reference/inspector))))))

;;;;;;
;;; t/name-value-list/inspector

(def (component e) t/name-value-list/inspector (inspector/basic t/name-value-list/presentation)
  ((slot-names nil :type list)))

(def layered-method collect-presented-slots ((component t/name-value-list/inspector) class prototype value)
  (bind (((:read-only-slots slot-names) component)
         (slots (class-slots class)))
    (if slot-names
        (filter-slots slot-names slots)
        slots)))

(def layered-method collect-presented-places ((component t/name-value-list/inspector) class prototype value)
  (mapcar [make-object-slot-place (component-value-of component) !1] value))

(def layered-method make-presented-place-group ((component t/name-value-list/inspector) class prototype value)
  (make-place-group nil value))

(def layered-methods make-content-presentation
  (:method ((component t/name-value-list/inspector) class prototype (value place-group))
    (make-instance 'place-group-list/name-value-list/inspector
                   :component-value value
                   :component-value-type (component-value-type-of component)
                   :edited (edited-component? component)
                   :editable (editable-component? component)))

  (:method ((component t/name-value-list/inspector) class prototype (value sequence))
    (make-instance 'sequence/list/inspector
                   :component-value value
                   :component-value-type (component-value-type-of component)
                   :edited (edited-component? component)
                   :editable (editable-component? component)))

  (:method ((component t/name-value-list/inspector) class prototype (value number))
    value)

  (:method ((component t/name-value-list/inspector) class prototype (value string))
    value)

  (:method ((component t/name-value-list/inspector) class prototype value)
    (make-instance 't/reference/inspector
                   :component-value value
                   :component-value-type (component-value-type-of component)
                   :action nil
                   :enabled #f
                   :edited (edited-component? component)
                   :editable (editable-component? component))))

;;;;;;
;;; place-group-list/name-value-list/inspector

(def (component e) place-group-list/name-value-list/inspector (inspector/basic place-group-list/name-value-list/presentation)
  ())

(def layered-method collect-presented-place-groups ((component place-group-list/name-value-list/inspector) class prototype (value place-group))
  (list value))

(def layered-method make-content-presentation ((component place-group-list/name-value-list/inspector) class prototype (value place-group))
  (make-instance 'place-group/name-value-group/inspector
                 :component-value value
                 :component-value-type (component-value-type-of component)
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; place-group/name-value-group/inspector

(def (component e) place-group/name-value-group/inspector (inspector/basic place-group/name-value-group/presentation)
  ())

(def layered-method make-content-presentation ((component place-group/name-value-group/inspector) class prototype (value object-slot-place))
  (make-instance 'place/name-value-pair/inspector
                 :component-value value
                 :component-value-type (component-value-type-of component)
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; place/name-value-pair/inspector

(def (component e) place/name-value-pair/inspector (inspector/basic place/name-value-pair/presentation)
  ())

(def layered-method make-value-presentation ((component place/name-value-pair/inspector) class prototype value)
  (make-instance 'place/value/inspector
                 :component-value value
                 :component-value-type (component-value-type-of component)
                 :edited (edited-component? component)
                 :editable (editable-component? component)))
