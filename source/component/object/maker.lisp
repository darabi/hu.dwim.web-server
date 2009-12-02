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
          (place/name/inspector                   ; inspect the name of a place
           (string/inspect                        ; inspect a string (alternator)
            (string/text/inspect                  ; inspect a string, display as text
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
  (list (delay-alternative-component-with-initargs 't/name-value-list/maker :component-value value)
        (delay-alternative-reference 't/reference/maker value)))

;;;;;;
;;; t/reference/maker

(def (component e) t/reference/maker (maker/basic t/reference/presentation)
  ())
