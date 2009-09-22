;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/presentation

(def (component e) t/presentation (alternator/widget)
  ((initial-alternative-type 't/reference/presentation)
   (default-alternative-type 't/detail/presentation)))

(def layered-method refresh-component :before ((-self- t/presentation))
  ;; TODO: shall we rather expect a list of symbols here, because all make-alternatives just do a few make-instance calls with the component-value
  (setf (alternatives-of -self-) (make-alternatives -self- (component-dispatch-class -self-) (component-dispatch-prototype -self-) (component-value-of -self-))))

;;;;;;
;;; t/reference/presentation

(def (component e) t/reference/presentation (reference/widget)
  ())

(def refresh-component t/reference/presentation
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (setf content (make-reference/content -self- class prototype component-value))))

(def (layered-function e) make-reference/content (component class prototype value)
  (:method ((component t/reference/presentation) class prototype value)
    (localized-instance-name value)))

;;;;;;
;;; t/detail/presentation

(def (component e) t/detail/presentation ()
  ())

;;;;;;
;;; t/name-value-list/presentation

(def (component e) t/name-value-list/presentation (t/detail/presentation content/widget)
  ())

(def refresh-component t/name-value-list/presentation
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (slots (collect-slot-value-list/slots -self- class prototype component-value))
         (content-value (if slots
                            (make-place-group nil (mapcar [make-object-slot-place component-value !1] slots))
                            component-value)))
    (if content
        (setf (component-value-of content) content-value)
        (setf content (make-slot-value-list/content -self- class prototype content-value)))))

;; TODO: rename
(def (layered-function e) collect-slot-value-list/slots (component class prototype value))

;; TODO: rename
(def (layered-function e) make-slot-value-list/content (component class prototype value))

;;;;;;
;;; place-group-list/name-value-list/presentation

(def (component e) place-group-list/name-value-list/presentation (name-value-list/widget)
  ())

(def refresh-component place-group-list/name-value-list/presentation
  (bind (((:slots component-value contents) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (content-values (collect-slot-value-group/slots -self- class prototype component-value)))
    (setf contents
          (iter (for content-value :in content-values)
                (for slot-value-group = nil #+nil (find)) ;; TODO:
                (if slot-value-group
                    (setf (component-value-of slot-value-group) content-value)
                    (setf slot-value-group (make-slot-value-list/content -self- class prototype content-value)))
                (collect slot-value-group)))))

;; TODO: rename
(def (layered-function e) collect-slot-value-group/slots (component class prototype value))

;;;;;;
;;; place-group/name-value-group/presentation

(def (component e) place-group/name-value-group/presentation (name-value-group/widget)
  ())

(def refresh-component place-group/name-value-group/presentation
  (bind (((:slots component-value contents title) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (setf title (name-of component-value)
          contents (iter (for place :in (places-of component-value))
                         (for slot-value-pair = nil #+nil(find)) ;; TODO:
                         (if slot-value-pair
                             (setf (component-value-of slot-value-pair) place)
                             (setf slot-value-pair (make-slot-value-group/content -self- class prototype place)))
                         (collect slot-value-pair)))))

(def (layered-function e) make-slot-value-group/content (component class prototype value))


;;;;;;
;;; place/name-value-pair/presentation

(def (component e) place/name-value-pair/presentation (name-value-pair/widget)
  ())

(def refresh-component place/name-value-pair/presentation
  (bind (((:slots component-value name value) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (instance (instance-of component-value))
         (slot (slot-of component-value)))
    (if component-value
        (progn
          (setf name (make-slot-value-pair/name -self- class prototype component-value))
          (if value
              (setf (component-value-of value) (make-object-slot-place instance slot))
              (setf value (make-slot-value-pair/value -self- class prototype component-value))))
        (setf name nil
              value nil))))

(def (layered-function e) make-slot-value-pair/name (component class prototype value)
  (:method ((component place/name-value-pair/presentation) class prototype value)
    (localized-slot-name (slot-of value)))

  (:method :in raw-names-layer ((component place/name-value-pair/presentation) class prototype value)
    (qualified-symbol-name (slot-definition-name (slot-of value)))))

(def (layered-function e) make-slot-value-pair/value (component class prototype value))








;;;;;;
;;; t/inspector

(def (component e) t/inspector (inspector/basic t/presentation)
  ())

(def layered-method make-alternatives ((component t/inspector) class prototype value)
  (list (delay-alternative-reference 't/reference/inspector value)
        (delay-alternative-component-with-initargs 't/name-value-list/inspector :component-value value)))

;;;;;;
;;; t/reference/inspector

(def (component e) t/reference/inspector (inspector/basic t/reference/presentation)
  ())

;;;;;;
;;; t/name-value-list/inspector

(def (component e) t/name-value-list/inspector (inspector/basic t/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-list/slots ((component t/name-value-list/inspector) class prototype value)
  (class-slots class))

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




;;;;;;
;;; sequence/inspector

(def (component e) sequence/inspector (t/inspector)
  ())

(def layered-method make-alternatives ((component sequence/inspector) class prototype value)
  (list (delay-alternative-reference 'sequence/reference/inspector value)
        (delay-alternative-component-with-initargs 'sequence/list/inspector :component-value value)
        (delay-alternative-component-with-initargs 'sequence/tree/inspector :component-value value)))

;;;;;;
;;; sequence/reference/inspector

(def (component e) sequence/reference/inspector (t/reference/inspector)
  ())

;;;;;;
;;; sequence/list/inspector

(def (component e) sequence/list/inspector (inspector/basic t/detail/presentation list/widget)
  ())

(def refresh-component sequence/list/inspector
  (bind (((:slots component-value contents) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (setf contents (iter (for content-value :in-sequence component-value)
                         (collect (make-list/element -self- class prototype content-value))))))

(def layered-function make-list/element (component class prototype value)
  (:method ((component sequence/list/inspector) class prototype value)
    (make-instance 't/element/inspector :component-value value)))

;;;;;;
;;; t/element/inspector

(def (component e) t/element/inspector (inspector/basic element/widget)
  ())

(def refresh-component t/element/inspector
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-element/content -self- class prototype component-value)))))

(def layered-function make-element/content (component class prototype value)
  (:method ((component t/element/inspector) class prototype value)
    (make-value-inspector value)))

;;;;;;
;;; t/tree/inspector

(def (component e) t/tree/inspector (inspector/basic tree/widget)
  ())

(def refresh-component t/tree/inspector
  (bind (((:slots root-nodes) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (root-node-values (ensure-sequence (component-value-of -self-))))
    (if root-nodes
        (foreach [setf (component-value-of !1) !2] root-nodes root-node-values)
        (setf root-nodes (mapcar [make-tree/root-node -self- dispatch-class dispatch-prototype !1] root-node-values)))))

(def (layered-function e) make-tree/root-node (component class prototype value))

;;;;;;
;;; sequence/tree/inspector

(def (component e) sequence/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component sequence/tree/inspector) class prototype value)
  (if (typep value 'sequence)
      (make-instance 't/node/inspector :component-value value)
      (make-value-inspector value)))

;;;;;;
;;; t/node/inspector

(def (component e) t/node/inspector (inspector/basic node/widget)
  ())

(def refresh-component t/node/inspector
  (bind (((:slots child-nodes content) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (children (collect-tree/children -self- dispatch-class dispatch-prototype component-value)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-node/content -self- dispatch-class dispatch-prototype component-value)))
    (if child-nodes
        (foreach [setf (component-value-of !1) !2] child-nodes children)
        (setf child-nodes (mapcar [make-node/child-node -self- dispatch-class dispatch-prototype !1] children)))))

(def (layered-function e) make-node/content (component class prototype value))

(def (layered-function e) make-node/child-node (component class prototype value))

(def (layered-function e) collect-tree/children (component class prototype value))

(def layered-method collect-tree/children ((component t/node/inspector) (class built-in-class) (prototype list) (value list))
  value)

(def layered-method collect-tree/children ((component t/node/inspector) class prototype value)
  nil)

(def layered-method make-node/content ((component t/node/inspector) class prototype value)
  (make-value-inspector value))

;;;;;;
;;; sequence/node/inspector

(def (component e) sequence/node/inspector (t/node/inspector)
  ())

(def layered-method make-node/child-node ((component sequence/node/inspector) class prototype value)
  (if (typep value 'sequence)
      (make-instance 'sequence/node/inspector :component-value value)
      (make-value-inspector value)))
























;;;;;;
;;; functional-tree

(def (class* e) functional-tree ()
  ((value :type t)
   (children-provider :type function)))

;;;;;;
;;; functional-tree/tree/inspector

(def (component e) functional-tree/tree/inspector (inspector/basic tree/widget)
  ())

(def refresh-component functional-tree/tree/inspector
  (bind (((:slots root-nodes) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (ensure-list (component-value-of -self-))))
    (setf root-nodes (make-tree/root-node -self- dispatch-class dispatch-prototype component-value))))

;;;;;;
;;; functional-tree/tree/inspector

(def (component e) functional-tree/node/inspector (inspector/basic node/widget)
  ())

(def refresh-component functional-tree/node/inspector
  (bind (((:slots child-nodes content) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (children (collect-tree/children -self- dispatch-class dispatch-prototype component-value)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-node/content -self- dispatch-class dispatch-prototype component-value)))
    (if child-nodes
        (foreach [setf (component-value-of !1) !2] child-nodes children)
        (setf child-nodes (mapcar [make-node/child-node -self- dispatch-class dispatch-prototype !1] children)))))

(def layered-method collect-tree/children ((component functional-tree/node/inspector) class prototype value)
  (funcall (children-provider-of value) (value-of value)))

















































;;;;;;
;;; t/filter

(def (component e) t/filter (filter/basic t/presentation)
  ())

(def layered-method make-alternatives ((component t/filter) class prototype value)
  (list (delay-alternative-reference 't/reference/filter value)
        (delay-alternative-component-with-initargs 't/name-value-list/filter :component-value value)))

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

(def generic collect-possible-filter-predicates (component)
  (:method ((self filter/abstract))
    nil))

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
         (possible-predicates (collect-possible-filter-predicates self)))
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