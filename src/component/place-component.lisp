;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place component

(def component place-component (editable-component)
  ((place nil)
   (content nil :type component)
   (command-bar nil :type component))
  (:documentation "Place component is resposible for being able to edit any value that is valid according to the type."))

(def constructor place-component ()
  (with-slots (edited content command-bar) -self-
    (setf content (make-place-component-content -self-)
          command-bar (when (subtypep (find-inspector-component-type-for-type (place-type (place-of -self-))) 'standard-object-inspector)
                        (make-instance 'command-bar-component :commands (list (make-revert-place-command -self-)
                                                                              (make-set-place-to-nil-command -self-)
                                                                              (make-set-place-to-find-instance-command -self-)
                                                                              (make-set-place-to-new-instance-command -self-)))))))

(def method (setf place-of) :after (new-value (self place-component))
  (setf (outdated-p self) #t))

(def render place-component ()
  (with-slots (edited content command-bar) -self-
    (if (and edited
             command-bar)
        (render-vertical-list (list command-bar content))
        (render content))))

(def function update-component-value-from-place (place component)
  (when (place-bound-p place)
    (bind ((value (value-at-place place)))
      (setf (component-value-of component)
            (if (prc::values-having-validity-p value)
                (prc::single-values-having-validity-value value)
                value)))))

(def function make-place-component-content (component)
  (bind ((place (place-of component)))
    (prog1-bind content
        (make-inspector-component (place-type place) :default-component-type 'reference-component)
      (update-component-value-from-place place content))))

(def (function e) make-special-variable-place-component (name type)
  (make-instance 'place-component :place (make-special-variable-place name type)))

(def (macro e) make-lexical-variable-place-component (name type)
  `(make-instance 'place-component :place (make-lexical-variable-place ,name ,type)))

(def (function e) make-standard-object-slot-value-place-component (instance slot-name)
  (make-instance 'place-component :place (make-slot-value-place instance (find-slot (class-of instance) slot-name))))

(def method map-editable-child-components ((self place-component) function)
  (when (place-editable-p (place-of self))
    (funcall function (content-of self))))

(def function revert-place-component-content (place-component)
  (update-component-value-from-place (place-of place-component) (content-of place-component)))

(def method refresh-component ((self place-component))
  (unless (edited-p self)
    (revert-place-component-content self)))

(def method revert-editing :after ((place-component place-component))
  (revert-place-component-content place-component))

(def method store-editing :after ((place-component place-component))
  (setf (value-at-place (place-of place-component)) (component-value-of (content-of place-component))))

(def layer* set-place-to-find-instance-layer ()
  ;; TODO: KLUDGE: FIXME: make this a special slot (this way it is not thread safe)
  ((place-component :type component)))

(def layered-method make-standard-object-row-inspector-commands :in-layer set-place-to-find-instance-layer :around
     ((component standard-object-row-inspector) (class standard-class) (instance standard-object))
     ;; TODO: check for being the row of the result component of the filter in the place component
  (bind ((place-component (place-component-of (current-layer))))
    (list* (command (icon select)
                    (make-action
                      (setf (component-value-of (content-of place-component)) (instance-of component))
                      (execute-command-bar-command (command-bar-of (find-ancestor-component-with-type component 'standard-object-filter)) 'back)))
           (call-next-method))))

(def (function e) make-set-place-to-find-instance-command (place-component)
  (make-replace-and-push-back-command place-component
                                      (with-active-layers ((set-place-to-find-instance-layer :place-component place-component))
                                        (make-filter-component (place-type (place-of place-component))))
                                      (list :icon (icon find))
                                      (list :icon (icon back))))

(def (function e) make-set-place-to-new-instance-command (place-component)
  (make-replace-and-push-back-command place-component
                                      (make-maker-component (place-type (place-of place-component)))
                                      (list :icon (icon new))
                                      (list :icon (icon back) :visible #t)))

(def (function e) make-set-place-to-nil-command (place-component)
  (command (icon set-to-nil)
           (make-action
             (setf (component-value-of (content-of place-component)) nil))))

(def (function e) make-revert-place-command (place-component)
  (make-instance 'command-component
                 :icon (icon revert)
                 :action (make-action (revert-place-component-content place-component))))
