;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/maker

(def (component e) t/maker (maker/basic t/presentation component-result/mixin)
  ()
  (:documentation "Generic factory version (all components are available):

(t/maker                                          ; maker for something (alternator)
 (t/place-list/maker                              ; maker for a list of places of something
  (place-list/maker                               ; maker for a list of places (alternator)
   (place-list/place-group-list/maker             ; maker for a grouping of a list of places
    (place-group-list/maker                       ; maker for a list of place groups (alternator)
     (place-group-list/name-value-list/maker      ; maker for a list of place groups, display as a name value list
      (place-group/maker                          ; maker for a group of places (alternator)
       (place-group/name-value-group/maker        ; maker for a group of places, display as a name value group
        (place/maker                              ; maker for a place (alternator)
         (place/name-value-pair/maker             ; maker for a place, display as a name value pair
          (place/name/maker                       ; maker the name of a place
           (string/maker                          ; maker a string (alternator)
            (string/text/maker                    ; maker a string, display as text
             string)))                            ; immediate
          (place/value/maker                      ; maker for the value of a place
           (t/maker))))                           ; maker for something (alternator)
        ...))
      ...))))))

Optimized factory configuration (default):

(t/maker                                          ; maker for something (alternator)
 (place-group-list/name-value-list/maker          ; maker for a list of place groups, display as a name value list
  (place-group/name-value-group/maker             ; maker for a group of places, display as a name value group
   (place/name-value-pair/maker                   ; maker for a place, display as a name value pair
    string                                        ; immediate
    (place/value/maker                            ; maker for the value of a place
     (t/maker)))                                  ; maker for something (alternator)
   ...)
  ...))
"))

(def subtype-mapper *maker-type-mapping* t t/maker)

(def layered-method make-alternatives ((component t/maker) class prototype value)
  (list (make-instance 't/name-value-list/maker
                       :component-value value
                       :component-value-type (component-value-type-of component))
        (make-instance 't/reference/maker
                       :component-value value
                       :component-value-type (component-value-type-of component))))

(def render-component t/maker
  <div ,(call-next-method)
    ,(render-result-for -self-)>)

(def layered-method make-command-bar-commands ((component t/maker) class prototype value)
  (optional-list* (make-execute-maker-command component class prototype value)
                  (call-next-method)))

(def (icon e) execute-maker)

(def layered-method make-execute-maker-command ((component t/maker) class prototype value)
  (make-replace-and-push-back-command (delay (result-of component))
                                      (delay (with-restored-component-environment component
                                               (make-result component class prototype (execute-maker component class prototype value))))
                                      (list :content (icon execute-maker) :default #t #+nil :ajax #+nil (ajax-of component))
                                      (list :content (icon navigate-back))))

(def layered-method make-result ((component t/maker) class prototype value)
  (make-inspector (class-name (component-dispatch-class component)) :value value))

(def (layered-function e) execute-maker (component class prototype value)
  (:method ((component t/maker) class prototype value)
    ;; TODO: copy the form builder from the old code base
    (make-instance class)))

;;;;;;
;;; t/reference/maker

(def (component e) t/reference/maker (maker/basic t/reference/presentation)
  ())

;;;;;;
;;; t/detail/maker

(def (component e) t/detail/maker (maker/abstract t/detail/presentation)
  ())

;;;;;;
;;; t/name-value-list/maker

(def (component e) t/name-value-list/maker (maker/basic t/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-list/slots ((component t/name-value-list/maker) class prototype value)
  (class-slots (component-dispatch-class component)))

(def layered-method make-slot-value-list/place-group ((component t/name-value-list/maker) class prototype value)
  (make-place-group nil (mapcar [make-object-slot-place (class-prototype (component-dispatch-class component)) !1] value)))

(def layered-methods make-slot-value-list/content
  (:method ((component t/name-value-list/maker) class prototype (value place-group))
    (make-instance 'place-group-list/name-value-list/maker
                   :component-value value
                   :component-value-type (component-value-type-of component)))

  (:method ((component t/name-value-list/maker) class prototype (value sequence))
    (make-instance 'sequence/list/maker
                   :component-value value
                   :component-value-type (component-value-type-of component)))

  (:method ((component t/name-value-list/maker) class prototype (value number))
    value)

  (:method ((component t/name-value-list/maker) class prototype (value string))
    value)

  (:method ((component t/name-value-list/maker) class prototype value)
    (make-instance 't/reference/maker
                   :component-value value
                   :component-value-type (component-value-type-of component)
                   :action nil :enabled #f)))

;;;;;;
;;; place-group-list/name-value-list/maker

(def (component e) place-group-list/name-value-list/maker (maker/basic place-group-list/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-group/slots ((component place-group-list/name-value-list/maker) class prototype (value place-group))
  (list value))

(def layered-method make-slot-value-list/content ((component place-group-list/name-value-list/maker) class prototype (value place-group))
  (make-instance 'place-group/name-value-group/maker
                 :component-value value
                 :component-value-type (component-value-type-of component)))

;;;;;;
;;; place-group/name-value-group/maker

(def (component e) place-group/name-value-group/maker (maker/basic place-group/name-value-group/presentation)
  ())

(def layered-method make-slot-value-group/content ((component place-group/name-value-group/maker) class prototype (value object-slot-place))
  (make-instance 'place/name-value-pair/maker
                 :component-value value
                 :component-value-type (component-value-type-of component)))

;;;;;;
;;; place/name-value-pair/maker

(def (component e) place/name-value-pair/maker (maker/basic place/name-value-pair/presentation)
  ())

(def layered-method make-slot-value-pair/value ((component place/name-value-pair/maker) class prototype value)
  (make-instance 'place/value/maker :component-value value))
