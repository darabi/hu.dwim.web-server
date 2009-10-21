;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/filter

(def (component e) t/filter (filter/basic t/presentation)
  ((result (empty/layout) :type component)
   (result-component-factory #'make-filter-result-inspector :type function))
  (:documentation "
;; generic version (all components are available)
(t/filter                                         ; filter for something (alternator)
 (t/place-list/filter                             ; filter for a list of places of something
  (place-list/filter                              ; filter for a list of places (alternator)
   (place-list/place-group-list/filter            ; filter for a grouping of a list of places
    (place-group-list/filter                      ; filter for a list of place groups (alternator)
     (place-group-list/name-value-list/filter     ; filter for a list of place groups, display as a name value list
      (place-group/filter                         ; filter for a group of places (alternator)
       (place-group/name-value-group/filter       ; filter for a group of places, display as a name value group
        (place/filter                             ; filter for a place (alternator)
         (place/name-value-pair/filter            ; filter for a place, display as a name value pair
          (place/name/inspector                   ; inspect the name of a place
           (string/inspect                        ; inspect a string (alternator)
            (string/string/inspect                ; inspect a string, display as a string
             string)))                            ; immediate
          (place/value/filter                     ; filter for the value of a place
           (t/filter))))                          ; filter for something (alternator)
        ...))
      ...))))))

;; optimized version (default factory configuration)
(t/filter                                         ; filter for something (alternator)
 (place-group-list/name-value-list/filter         ; filter for a list of place groups, display as a name value list
  (place-group/name-value-group/filter            ; filter for a group of places, display as a name value group
   (place/name-value-pair/filter                  ; filter for a place, display as a name value pair
    string                                        ; immediate
    (place/value/filter                           ; filter for the value of a place
     (t/filter)))                                 ; filter for something (alternator)
   ...)
  ...))
"))

(def layered-method make-alternatives ((component t/filter) class prototype value)
  (list (delay-alternative-reference 't/reference/filter value)
        (delay-alternative-component-with-initargs 't/name-value-list/filter :component-value value)))

(def render-component t/filter
  <div ,(call-next-method)
       ,(render-component (result-of -self-))>)

(def layered-method make-command-bar-commands ((component t/filter) class prototype value)
  (optional-list* (make-execute-filter-command component class prototype value)
                  (call-next-method)))

(def (icon e) execute-filter)

(def layered-method make-execute-filter-command ((component t/filter) class prototype value)
  (make-replace-and-push-back-command (result-of component)
                                      (delay (with-restored-component-environment component
                                               (funcall (result-component-factory-of component) component class prototype
                                                        (execute-filter component class prototype value))))
                                      (list :content (icon execute-filter) :default #t)
                                      (list :content (icon back))))

(def (layered-function e) make-filter-result-inspector (component class prototype value)
  (:method ((filter t/filter) class prototype (value list))
    (make-viewer `(list ,(class-name (component-value-of filter))) value))

  (:method :around ((filter t/filter) class prototype  value)
    (prog1-bind component
        (call-next-method)
      (unless value
        (add-component-warning-message component #"no-matches-were-found")))))

(def (layered-function e) execute-filter (component class prototype value)
  (:method ((component t/filter) class prototype value)
    (execute-filter (content-of component) class prototype value)))

;;;;;;
;;; t/reference/filter

(def (component e) t/reference/filter (filter/basic t/reference/presentation)
  ())

;;;;;;
;;; t/name-value-list/filter

(def (component e) t/name-value-list/filter (filter/basic t/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-list/slots ((component t/name-value-list/filter) class prototype value)
  (class-slots value))

#+sbcl
(def layered-method execute-filter ((component t/name-value-list/filter) class prototype value)
  (bind (#+nil
         (slot-values (mappend #'slot-values-of (slot-value-groups-of component)))
         #+nil
         (predicates (iter (for slot-value :in slot-values)
                           (for predicate = (bind ((slot-name (slot-definition-name (slot-of slot-value)))
                                                   (place-filter (value-of slot-value))
                                                   (value-component (content-of place-filter))
                                                   (predicate-function (bind ((function (ensure-function (predicate-function place-filter class (selected-predicate-of place-filter)))))
                                                                         (if (negated-p place-filter)
                                                                             (complement function)
                                                                             function))))
                                              (when (use-in-filter? place-filter)
                                                (lambda (instance)
                                                  (bind ((instance-class (class-of instance))
                                                         (slot (find-slot instance-class slot-name)))
                                                    (and (slot-boundp-using-class instance-class instance slot)
                                                         (funcall predicate-function
                                                                  (slot-value-using-class instance-class instance slot)
                                                                  (component-value-of value-component))))))))
                           (when predicate
                             (collect predicate)))))
    (bind ((instances nil))
      (map-filter-input component class prototype value
                        (lambda (instance)
                          (bind ((instance-class (find-class 'standard-class) #+nil(class-of instance)))
                            (when (and (typep instance class)
                                       (not (eq instance (class-prototype instance-class)))
                                       #+nil
                                       (every (lambda (predicate)
                                                (funcall predicate instance))
                                              predicates))
                              (push instance instances)))))
      instances)))

(def (layered-function e) map-filter-input (component class prototype value function)
  (:method ((component t/name-value-list/filter) class prototype value function)
    (break)
    (sb-vm::map-allocated-objects
     (lambda (instance type size)
       (declare (ignore type size))
       (funcall function instance))
     :dynamic #t))

  (:method ((component t/name-value-list/filter) (class class) prototype (value class) function)
    (maphash-keys (lambda (key)
                    (awhen (find-class key #f)
                      (funcall function it)))
                  sb-kernel::*classoid-cells*)))

;; TODO: rename
(def layered-methods make-slot-value-list/content
  (:method ((component t/name-value-list/filter) class prototype (value place-group))
    (make-instance 'place-group-list/name-value-list/filter :component-value value))

  (:method ((component t/name-value-list/filter) class prototype (value sequence))
    (make-instance 'sequence/list/filter :component-value value))

  (:method ((component t/name-value-list/filter) class prototype (value number))
    value)

  (:method ((component t/name-value-list/filter) class prototype (value string))
    value)

  (:method ((component t/name-value-list/filter) class prototype value)
    (make-instance 't/reference/filter :component-value value :action nil :enabled #f)))

;;;;;;
;;; place-group-list/name-value-list/filter

(def (component e) place-group-list/name-value-list/filter (filter/basic place-group-list/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-group/slots ((component place-group-list/name-value-list/filter) class prototype (value place-group))
  (list value))

(def layered-method make-slot-value-list/content ((component place-group-list/name-value-list/filter) class prototype (value place-group))
  (make-instance 'place-group/name-value-group/filter :component-value value))

;;;;;;
;;; place-group/name-value-group/filter

(def (component e) place-group/name-value-group/filter (filter/basic place-group/name-value-group/presentation)
  ())

(def layered-method make-slot-value-group/content ((component place-group/name-value-group/filter) class prototype (value object-slot-place))
  (make-instance 'place/name-value-pair/filter :component-value value))

;;;;;;
;;; place/name-value-pair/filter

;; TODO: move these slots down one level?
(def (component e) place/name-value-pair/filter (filter/basic place/name-value-pair/presentation)
  ((use-in-filter #f :type boolean)
   (use-in-filter-id)
   (negated #f :type boolean)
   (selected-predicate nil :type symbol)))

(def layered-method make-slot-value-pair/value ((component place/name-value-pair/filter) class prototype value)
  (make-instance 'place/value/filter :component-value value))

;; TODO: do we need this parentism
(def method use-in-filter? ((self parent/mixin))
  (use-in-filter? (parent-component-of self)))

;; TODO: do we need this parentism
(def method use-in-filter-id-of ((self parent/mixin))
  (use-in-filter-id-of (parent-component-of self)))

(def method use-in-filter-id-of ((self place/name-value-pair/filter))
  (generate-frame-unique-string))

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

(def method collect-filter-predicates ((self filter/abstract))
  nil)

(def function localize-predicate (predicate)
  (lookup-resource (string+ "predicate." (symbol-name predicate))))

(def function predicate-icon-style-class (predicate)
  (ecase predicate
    (equal "equal-predicate-icon")
    (like "like-predicate-icon")
    (less-than "less-than-predicate-icon")
    (less-than-or-equal "less-than-or-equal-predicate-icon")
    (greater-than "greater-than-predicate-icon")
    (greater-than-or-equal "greater-than-or-equal-predicate-icon")))

(def function render-filter-predicate-for (self)
  (bind (((:slots negated selected-predicate) self)
         ;; TODO: KLUDGE: don't look down this deep
         (possible-predicates (collect-filter-predicates (content-of (aprog1 (value-of self)
                                                                       (ensure-refreshed it))))))
    (if possible-predicates
        (progn
          (unless selected-predicate
            (setf selected-predicate (first possible-predicates)))
          <td ,(render-checkbox-field negated
                                      :value-sink (lambda (value) (setf negated value))
                                      :checked-class "icon negated-predicate-icon"
                                      :unchecked-class "icon ponated-predicate-icon")>
          <td ,(if (length= 1 possible-predicates)
                   <div (:class ,(predicate-icon-style-class (first possible-predicates)))>
                   (render-popup-menu-select-field (localize-predicate selected-predicate)
                                                   (mapcar #'localize-predicate possible-predicates)
                                                   :value-sink (lambda (value)
                                                                 (setf selected-predicate (find value possible-predicates :key #'localize-predicate :test #'string=)))
                                                   :classes (mapcar #'predicate-icon-style-class possible-predicates)))>)
        <td (:colspan 2)>)))

(def function render-use-in-filter-marker-for (self)
  (bind ((id (generate-frame-unique-string)))
    (setf (use-in-filter-id-of self) id)
    <td ,(render-checkbox-field (use-in-filter? self)
                                :id id
                                :value-sink (lambda (value) (setf (use-in-filter? self) value))
                                :checked-class "icon use-in-filter-icon"
                                :unchecked-class "icon ignore-in-filter-icon")>))
