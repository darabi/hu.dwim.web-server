;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; dictionary/alternator/inspector

(def (component e) dictionary/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null dictionary) dictionary/alternator/inspector)

(def layered-method make-alternatives ((component dictionary/alternator/inspector) (class standard-class) (prototype dictionary) (value dictionary))
  (list* (make-instance 'dictionary/name-list/inspector :component-value value)
         (make-instance 'dictionary/documentation/inspector :component-value value)
         (call-next-layered-method)))

;;;;;;
;;; t/reference/inspector

(def layered-method make-reference-content ((component t/reference/inspector) (class standard-class) prototype (value dictionary))
  (string+ "Dictionary: " (call-next-layered-method)))

;;;;;;
;;; dictionary/documentation/inspector

(def (component e) dictionary/documentation/inspector (t/documentation/inspector)
  ())

(def method make-documentation ((component dictionary/documentation/inspector) class prototype (value dictionary))
  (documentation-of value))

;;;;;;
;;; dictionary/name-list/inspector

(def (component e) dictionary/name-list/inspector (t/detail/inspector contents/widget title/mixin)
  ())

(def refresh-component dictionary/name-list/inspector
  (bind (((:slots contents component-value) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-)))
    (setf contents (mapcar (lambda (name)
                             (make-dictionary/name-content -self- dispatch-class dispatch-prototype name))
                           (definition-names-of component-value)))))

(def render-xhtml dictionary/name-list/inspector
  (with-render-style/component (-self-)
    (render-title-for -self-)
    (render-contents-for -self-)))

(def layered-method make-title ((self dictionary/name-list/inspector) class prototype (value dictionary))
  (string+ "Dictionary: " (localized-instance-name value)))

(def generic make-dictionary/name-content (component class prototype value)
  (:method ((component dictionary/name-list/inspector) class prototype value)
    (make-instance 'symbol/definition-name/inspector :component-value value)))
