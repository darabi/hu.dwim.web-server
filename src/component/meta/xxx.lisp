;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)





;;;;;;
;;; t/reference/inspector

(def (component e) t/reference/inspector (inspector/basic reference/widget)
  ())

;;;;;;
;;; t/presentation

(def (component e) t/presentation (alternator/widget)
  ((initial-alternative-type 't/reference/presentation)))

(def layered-method refresh-component :before ((-self- t/presentation))
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
;;; t/slot-value-list/presentation

(def (component e) t/slot-value-list/presentation (content/widget)
  ())

(def refresh-component t/slot-value-list/presentation
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (slots (collect-slot-value-list/slots -self- class prototype component-value))
         (content-value (if slots
                            (make-slot-value-place-list component-value slots)
                            component-value)))
    (if content
        (setf (component-value-of content) content-value)
        (setf content (make-slot-value-list/content -self- class prototype content-value)))))

;; TODO: rename
(def (layered-function e) collect-slot-value-list/slots (component class prototype value))

;; TODO: rename
(def (layered-function e) make-slot-value-list/content (component class prototype value))

;;;;;;
;;; slot-value-place-list/name-value-list/presentation

(def (component e) slot-value-place-list/name-value-list/presentation (name-value-list/widget)
  ())

(def refresh-component slot-value-place-list/name-value-list/presentation
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
;;; slot-value-place-list/name-value-group/presentation

(def (component e) slot-value-place-list/name-value-group/presentation (name-value-group/widget)
  ())

(def refresh-component slot-value-place-list/name-value-group/presentation
  (bind (((:slots component-value contents title) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (instance (instance-of component-value))
         (content-values (mapcar (lambda (slot)
                                   (make-slot-value-place instance slot))
                                 (slots-of component-value))))
    (setf title (name-of component-value)
          contents (iter (for place :in content-values)
                         (for slot-value-pair = nil #+nil(find)) ;; TODO:
                         (if slot-value-pair
                             (setf (component-value-of slot-value-pair) place)
                             (setf slot-value-pair (make-slot-value-group/content -self- class prototype place)))
                         (collect slot-value-pair)))))

(def (layered-function e) make-slot-value-group/content (component class prototype value))

;;;;;;
;;; slot-value-place/name-value-pair/presentation

(def (component e) slot-value-place/name-value-pair/presentation (name-value-pair/widget)
  ())

(def refresh-component slot-value-place/name-value-pair/presentation
  (bind (((:slots component-value name value) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (instance (instance-of component-value))
         (slot (slot-of component-value)))
    (if component-value
        (progn
          (setf name (make-slot-value-pair/name -self- class prototype component-value))
          (if value
              (setf (component-value-of value) (make-slot-value-place instance slot))
              (setf value (make-slot-value-pair/value -self- class prototype component-value))))
        (setf name nil
              value nil))))

(def (layered-function e) make-slot-value-pair/name (component class prototype value)
  (:method ((component slot-value-place/name-value-pair/presentation) class prototype value)
    (localized-slot-name (slot-of value)))

  (:method :in raw-names-layer ((component slot-value-place/name-value-pair/presentation) class prototype value)
    (qualified-symbol-name (slot-definition-name (slot-of value)))))

(def (layered-function e) make-slot-value-pair/value (component class prototype value))

;;;;;;
;;; slot-value-place/content/presentation

(def (component e) slot-value-place/content/presentation (content/widget)
  ())

(def refresh-component slot-value-place/content/presentation
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-slot-value/content -self- class prototype component-value)))))

(def (layered-function e) make-slot-value/content (component class prototype value))








;;;;;;
;;; t/inspector

(def (component e) t/inspector (inspector/basic t/presentation)
  ((default-alternative-type 't/slot-value-list/inspector)))

(def layered-method make-alternatives ((component t/inspector) class prototype value)
  (list (delay-alternative-reference 't/reference/inspector value)
        (delay-alternative-component-with-initargs 't/slot-value-list/inspector :component-value value)))

;;;;;;
;;; t/reference/inspector

(def (component e) t/reference/inspector (inspector/basic t/reference/presentation)
  ())

;;;;;;
;;; t/slot-value-list/inspector

;; TODO: rename
(def (component e) t/slot-value-list/inspector (inspector/basic t/slot-value-list/presentation)
  ())

(def layered-method collect-slot-value-list/slots ((component t/slot-value-list/inspector) class prototype value)
  (class-slots class))

;; TODO: rename
(def layered-methods make-slot-value-list/content
  (:method ((component t/slot-value-list/inspector) class prototype (value slot-value-place-list))
    (make-instance 'slot-value-place-list/name-value-list/inspector :component-value value))

  (:method ((component t/slot-value-list/inspector) class prototype (value sequence))
    (make-instance 'sequence/list/inspector :component-value value))

  (:method ((component t/slot-value-list/inspector) class prototype (value number))
    value)

  (:method ((component t/slot-value-list/inspector) class prototype (value string))
    value)

  (:method ((component t/slot-value-list/inspector) class prototype value)
    (make-instance 't/reference/inspector :component-value value :action nil :enabled #f)))

;;;;;;
;;; slot-value-place-list/name-value-list/inspector

(def (component e) slot-value-place-list/name-value-list/inspector (inspector/basic slot-value-place-list/name-value-list/presentation)
  ())

(def layered-method collect-slot-value-group/slots ((component slot-value-place-list/name-value-list/inspector) class prototype (value slot-value-place-list))
  (list value))

(def layered-method make-slot-value-list/content ((component slot-value-place-list/name-value-list/inspector) class prototype (value slot-value-place-list))
  (make-instance 'slot-value-place-list/name-value-group/inspector :component-value value))

;;;;;;
;;; slot-value-place-list/name-value-group/inspector

(def (component e) slot-value-place-list/name-value-group/inspector (inspector/basic slot-value-place-list/name-value-group/presentation)
  ())

(def layered-method make-slot-value-group/content ((component slot-value-place-list/name-value-group/inspector) class prototype (value slot-value-place))
  (make-instance 'slot-value-place/name-value-pair/inspector :component-value value))

;;;;;;
;;; slot-value-place/name-value-pair/inspector

(def (component e) slot-value-place/name-value-pair/inspector (inspector/basic slot-value-place/name-value-pair/presentation)
  ())

(def layered-method make-slot-value-pair/value ((component slot-value-place/name-value-pair/inspector) class prototype value)
  ;; TODO: add extra level of indirection called slot-value-place/content/inspector
  (make-instance 'slot-value-place/content/inspector :component-value value))

;;;;;;
;;; slot-value-place/content/inspector

;; TODO:
(def (component e) slot-value-place/content/inspector (inspector/basic slot-value-place/content/presentation)
  ())

(def layered-method make-slot-value/content ((component slot-value-place/content/inspector) class prototype value)
  (make-inspector (place-type value)
                  ;; TODO: handle unbound in a better way
                  (if (place-bound? value)
                      (value-at-place value)
                      "<unbound>")))



















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

(def (component e) sequence/list/inspector (inspector/basic list/widget)
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
