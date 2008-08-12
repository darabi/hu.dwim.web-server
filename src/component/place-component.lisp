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
                        (make-instance 'command-bar-component :commands (list (make-find-instance-command -self-)))))))

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

(def function revert-place-component-content (place-component)
  (update-component-value-from-place (place-of place-component) (content-of place-component)))

(def method refresh-component ((self place-component))
  (unless (edited-p self)
    (revert-place-component-content self)))

(def method revert-editing :after ((place-component place-component))
  (revert-place-component-content place-component))

(def method store-editing :after ((place-component place-component))
  (setf (value-at-place (place-of place-component)) (component-value-of (content-of place-component))))

(def (function e) make-find-instance-command (place-component)
  (make-replace-and-push-back-command place-component (make-filter-component (place-type (place-of place-component)))
                                      (list :icon (icon find))
                                      (list :icon (icon back))))

(def (function e) make-makunbound-command (place-component)
  (make-replace-command (delay (content-of place-component))
                        (delay (make-place-component-content place-component))
                        :icon (icon makunbound :label "Makunbound")
                        :visible (delay (not (typep (content-of place-component) 'unbound-component)))))

(def (function e) make-revert-command (place-component)
  (make-instance 'command-component
                 :icon (icon revert :label "Revert")
                 :action (make-action (revert-place-component-content place-component))))
