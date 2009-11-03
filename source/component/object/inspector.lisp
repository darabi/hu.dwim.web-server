;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/inspector

(def (component e) t/inspector (inspector/basic t/presentation)
  ()
  (:documentation "
;; generic version (all components are available)
(t/inspector                                      ; inspect something (alternator)
 (t/place-list/inspector                          ; inspect a list of places of something
  (place-list/inspector                           ; inspect a list of places (alternator)
   (place-list/place-group-list/inspector         ; inspect a grouping of a list of places
    (place-group-list/inspector                   ; inspect a list of place groups (alternator)
     (place-group-list/name-value-list/inspector  ; inspect a list of place groups, display as a name value list
      (place-group/inspector                      ; inspect a group of places (alternator)
       (place-group/name-value-group/inspector    ; inspect a group of places, display as a name value group
        (place/inspector                          ; inspect a place (alternator)
         (place/name-value-pair/inspector         ; inspect a place, display as a name value pair
          (place/name/inspector                   ; inspect the name of a place
           (string/inspector                      ; inspect a string (alternator)
            (string/string/inspector              ; inspect a string, display as a string
             string)))                            ; immediate
          (place/value/inspector                  ; inspect the value of a place
           (t/inspector))))                       ; inspect something (alternator)
        ...))
      ...))))))

;; optimized version (default factory configuration)
(t/inspector                                      ; inspect something (alternator)
 (place-group-list/name-value-list/inspector      ; inspect a list of place groups, display as a name value list
  (place-group/name-value-group/inspector         ; inspect a group of places, display as a name value group
   (place/name-value-pair/inspector               ; inspect a place, display as a name value pair
    string                                        ; immediate
    (place/value/inspector                        ; inspect the value of a place
     (t/inspector)))                              ; inspect something (alternator)
   ...)
  ...))

(string/inspector
 (string/string/inspector)
 (string/character-vector/inspector)
 ...)
"))

(def layered-method make-alternatives ((component t/inspector) class prototype value)
  (list (delay-alternative-component-with-initargs 't/name-value-list/inspector :component-value value)
        (delay-alternative-reference 't/reference/inspector value)))

;;;;;;
;;; t/reference/inspector

(def (component e) t/reference/inspector (inspector/basic t/reference/presentation)
  ())

;;;;;
;;; t/documentation/inspector

(def (component e) t/documentation/inspector (inspector/basic t/detail/presentation content/widget)
  ())

(def refresh-component t/documentation/inspector
  (bind (((:slots content component-value) -self-))
    (setf content (make-documentation -self- (component-dispatch-class -self-) (component-dispatch-prototype -self-) component-value))))

(def generic make-documentation (component class prototype value)
  (:method ((component t/documentation/inspector) class prototype value)
    (documentation value t)))


;;;;;;
;;; t/name-value-list/inspector

(def (component e) t/name-value-list/inspector (inspector/basic t/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-list/slots ((component t/name-value-list/inspector) class prototype value)
  (class-slots class))

(def layered-method make-slot-value-list/place-group ((component t/name-value-list/inspector) class prototype value)
  (make-place-group nil (mapcar [make-object-slot-place (component-value-of component) !1] value)))

;; TODO: rename
(def layered-methods make-slot-value-list/content
  (:method ((component t/name-value-list/inspector) class prototype (value place-group))
    (make-instance 'place-group-list/name-value-list/inspector :component-value value))

  (:method ((component t/name-value-list/inspector) class prototype (value sequence))
    (make-instance 'sequence/list/inspector :component-value value))

  (:method ((component t/name-value-list/inspector) class prototype (value number))
    value)

  (:method ((component t/name-value-list/inspector) class prototype (value string))
    value)

  (:method ((component t/name-value-list/inspector) class prototype value)
    (make-instance 't/reference/inspector :component-value value :action nil :enabled #f)))

;;;;;;
;;; place-group-list/name-value-list/inspector

(def (component e) place-group-list/name-value-list/inspector (inspector/basic place-group-list/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-group/slots ((component place-group-list/name-value-list/inspector) class prototype (value place-group))
  (list value))

(def layered-method make-slot-value-list/content ((component place-group-list/name-value-list/inspector) class prototype (value place-group))
  (make-instance 'place-group/name-value-group/inspector :component-value value))

;;;;;;
;;; place-group/name-value-group/inspector

(def (component e) place-group/name-value-group/inspector (inspector/basic place-group/name-value-group/presentation)
  ())

(def layered-method make-slot-value-group/content ((component place-group/name-value-group/inspector) class prototype (value object-slot-place))
  (make-instance 'place/name-value-pair/inspector :component-value value))

;;;;;;
;;; place/name-value-pair/inspector

(def (component e) place/name-value-pair/inspector (inspector/basic place/name-value-pair/presentation)
  ())

(def layered-method make-slot-value-pair/value ((component place/name-value-pair/inspector) class prototype value)
  (make-instance 'place/value/inspector :component-value value))
