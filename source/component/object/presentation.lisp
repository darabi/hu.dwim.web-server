;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/presentation

(def (component e) t/presentation (presentation/abstract alternator/widget)
  ((initial-alternative-type 't/detail/presentation)
   (default-alternative-type 't/detail/presentation))
  (:documentation "Presentation for all types."))

(def layered-method refresh-component :before ((-self- t/presentation))
  (bind (((:slots alternatives) -self-)
         (component-value (component-value-of -self-)))
    (if alternatives
        (foreach [setf (component-value-of !1) component-value] alternatives)
        (setf alternatives (make-alternatives -self- (component-dispatch-class -self-) (component-dispatch-prototype -self-) component-value)))))

;;;;;;
;;; t/reference/presentation

(def (component e) t/reference/presentation (presentation/abstract reference/widget)
  ())

(def refresh-component t/reference/presentation
  (bind (((:slots component-value content subject-component enabled-component action) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (setf content (icon/widget expand-from-reference :label (make-reference-content -self- class prototype component-value)))
    (setf subject-component (delay (parent-component-of -self-)))
    (setf action (make-action (execute-replace -self- (delay (find-default-alternative-component (parent-component-of -self-))))))
    (setf enabled-component (authorize-operation *application* `(make-switch-to-alternative-command :class ,class :instance ,component-value :alternative ,(class-name (class-of -self-)))))))

(def layered-method make-reference-content (component class prototype value)
  (localized-instance-name value))

;;;;;;
;;; t/detail/presentation

(def (component e) t/detail/presentation (presentation/abstract)
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
         (content-value (make-slot-value-list/place-group -self- class prototype slots)))
    (if content
        (setf (component-value-of content) content-value)
        (setf content (make-slot-value-list/content -self- class prototype content-value)))))

;; TODO: rename
(def (layered-function e) collect-slot-value-list/slots (component class prototype value)
  (:method :around ((component filter/abstract) class prototype value)
    (collect-if [authorize-operation *application* `(filter-slot-value :class ,class :prototype ,prototype :slot ,!1)]
                (call-next-layered-method)))

  (:method :around ((component inspector/abstract) class prototype value)
    (collect-if [authorize-operation *application* `(inspect-slot-value :class ,class :prototype ,prototype :slot ,!1)]
                (call-next-layered-method))))

;; TODO: rename
(def (layered-function e) make-slot-value-list/content (component class prototype value))

(def (layered-function e) make-slot-value-list/place-group (component class prototype value))

;;;;;;
;;; place-group-list/name-value-list/presentation

(def (component e) place-group-list/name-value-list/presentation (t/detail/presentation name-value-list/widget)
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

(def (component e) place-group/name-value-group/presentation (t/detail/presentation name-value-group/widget)
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

(def (component e) place/name-value-pair/presentation (t/detail/presentation name-value-pair/widget)
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

  (:method :in raw-name-layer ((component place/name-value-pair/presentation) class prototype value)
    (fully-qualified-symbol-name (slot-definition-name (slot-of value)))))

(def (layered-function e) make-slot-value-pair/value (component class prototype value))
