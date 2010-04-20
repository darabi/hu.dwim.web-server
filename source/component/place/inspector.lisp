;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/inspector

(def (component e) place/inspector (t/inspector place/presentation)
  ()
  (:documentation "An PLACE/INSPECTOR displays or edits existing values of a TYPE at a PLACE."))

(def subtype-mapper *inspector-type-mapping* place place/inspector)

(def layered-method make-alternatives ((component place/inspector) class prototype value)
  (list (make-instance 'place/value/inspector
                       :component-value value
                       :editable (editable-component? component)
                       :edited (edited-component? component))
        (make-instance 'place/reference/inspector
                       :component-value value
                       :editable (editable-component? component)
                       :edited (edited-component? component))))

;;;;;;
;;; place/reference/inspector

(def (component e) place/reference/inspector (t/reference/inspector place/reference/presentation)
  ())

;;;;;;
;;; place/value/inspector

(def (component e) place/value/inspector (inspector/basic place/value/presentation)
  ())

(def layered-method make-content-presentation ((component place/value/inspector) class prototype value)
  (if (authorize-operation *application* '(make-content-presentation))
      (if (place-bound? value)
          (make-inspector (place-type value)
                          :value (place-value value)
                          :editable (editable-component? component)
                          :edited (edited-component? component)
                          :initial-alternative-type 't/reference/inspector)
          (make-instance 'unbound/inspector))
      (empty/layout)))

;; TODO: dispatch on generic place/??/inspector base class
(def method editable-component? ((self place/value/inspector))
  (and (call-next-method)
       (place-editable? (component-value-of self))))

;; TODO: dispatch on generic place/??/inspector base class
(def method store-editing :after ((self place/value/inspector))
  (bind ((place (component-value-of self))
         (content (content-of self)))
    (handler-bind ((type-error (lambda (error)
                                 ;; TODO localize
                                 (add-component-error-message self "Nem megfelelő adat")
                                 (abort-interaction)
                                 (continue error))))
      (reuse-component-value self (component-dispatch-class self) (component-dispatch-prototype self) place)
      (setf (place-value place) (component-value-of content)))))

(def method revert-editing :after ((self place/value/inspector))
  (bind ((place (component-value-of self))
         (content (content-of self)))
    (reuse-component-value self (component-dispatch-class self) (component-dispatch-prototype self) place)
    ;; TODO: what if the place is unbound?
    (when (place-bound? place)
      (setf (component-value-of content) (place-value place)))))
