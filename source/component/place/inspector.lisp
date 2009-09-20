;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place inspector

(def (component e) place-inspector (place-component inspector/abstract editable/mixin)
  ((place nil :type place)))

(def render-xhtml place-inspector
  (bind (((:slots edited content command-bar) -self-))
    (if (and edited
             command-bar)
        (render-vertical-list (list command-bar content))
        (render-component content))))

(def method the-type-of ((self place-inspector))
  (place-type (place-of self)))

(def method make-place-component-content ((self place-inspector))
  (bind (((:slots place edited content command-bar) self))
    (if content
        (progn
          (unless (edited? content)
            (revert-place-inspector-content self))
          content)
        (if (typep place 'instance-slot-place)
            (bind ((instance (instance-of place)))
              (make-place-inspector-content self (class-of instance) instance (slot-of place)))
            (make-place-inspector-content self nil nil nil)))))

(def method (setf place-of) :after (new-value (self place-inspector))
  (setf (outdated-p self) #t))

(def function update-component-value-from-place (place component)
  (when (place-bound? place)
    (bind ((value (value-at-place place)))
      (setf (component-value-of component) value))))

(def (layered-function e) make-place-inspector-content (component class instance slot)
  (:method ((component place-inspector) class instance slot)
    (bind ((place (place-of component)))
      (prog1-bind content
          (make-inspector (place-type place) :initial-alternative-type 'reference-component)
        (update-component-value-from-place place content)))))

(def method editable-component? ((self place-inspector))
  (and (place-editable? (place-of self))
       (editable-component? (content-of self))))

(def function revert-place-inspector-content (place-inspector)
  (update-component-value-from-place (place-of place-inspector) (content-of place-inspector)))

(def method revert-editing :after ((place-inspector place-inspector))
  (revert-place-inspector-content place-inspector))

(def method store-editing :after ((place-inspector place-inspector))
  (bind ((place (place-of place-inspector))
         (value (place-component-value-of (content-of place-inspector))))
    (handler-bind ((type-error (lambda (error)
                                 (add-component-error-message place-inspector "Nem megfelelő adat")
                                 (abort-interaction)
                                 (continue error))))
      (setf (value-at-place place) value))))

(def (function e) make-revert-place-command (component)
  (command ()
    (icon revert)
    (make-component-action component
      (revert-place-inspector-content component))))
