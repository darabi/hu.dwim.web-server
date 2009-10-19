;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

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
;;; sequence/tree/inspector

(def (component e) sequence/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component sequence/tree/inspector) class prototype value)
  (if (typep value 'sequence)
      (make-instance 't/node/inspector :component-value value)
      (make-value-inspector value)))

;;;;;;
;;; sequence/node/inspector

(def (component e) sequence/node/inspector (t/node/inspector)
  ())

(def layered-method make-node/child-node ((component sequence/node/inspector) class prototype value)
  (if (typep value 'sequence)
      (make-instance 'sequence/node/inspector :component-value value)
      (make-value-inspector value)))