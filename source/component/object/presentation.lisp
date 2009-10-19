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