;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place component

(def component place-component (content-component)
  ((command-bar nil :type component)))

(def render place-component ()
  (with-slots (content command-bar) -self-
    (if command-bar
        (render-vertical-list (list command-bar content))
        (call-next-method))))

(def generic place-component-value-of (component)
  (:method ((self component))
    (component-value-of self)))

(def layer* set-place-to-find-instance-layer ()
  ;; TODO: KLUDGE: FIXME: make this a special slot (this way it is not thread safe)
  ((place-component :type component)))

(def layered-method make-standard-object-row-inspector-commands :in-layer set-place-to-find-instance-layer :around
     ((component standard-object-row-inspector) (class standard-class) (instance standard-object))
     ;; TODO: check for being the row of the result component of the filter in the place component
  (bind ((place-component
          (parent-component-of (find-ancestor-component-with-type component 'standard-object-filter))
          #+nil
          (place-component-of (current-layer))))
    (list* (command (icon select)
                    (make-action
                      (execute-command-bar-command (command-bar-of (find-ancestor-component-with-type component 'standard-object-filter)) 'back)
                      (setf (component-value-of (content-of place-component)) (instance-of component))))
           (call-next-method))))

(def (function e) make-set-place-to-nil-command (place-component)
  (command (icon set-to-nil)
           (make-action
             (setf (component-value-of (content-of place-component)) nil))
           :visible (delay (bind ((content (content-of place-component)))
                             (and (typep content 'inspector-component)
                                  (not (null (component-value-of content))))))))

(def (function e) make-set-place-to-find-instance-command (place-component)
  (make-replace-and-push-back-command (delay (content-of place-component))
                                      (with-active-layers ((set-place-to-find-instance-layer :place-component place-component))
                                        (make-filter-component (the-type-of place-component)))
                                      (list :icon (icon find))
                                      (list :icon (icon back))))

(def (function e) make-set-place-to-new-instance-command (place-component)
  (make-replace-and-push-back-command (delay (content-of place-component))
                                      (make-maker-component (the-type-of place-component))
                                      (list :icon (icon new) )
                                      (list :icon (icon back) :visible #t)))

;;;;;;
;;; Place inspector

(def component place-inspector (place-component inspector-component editable-component)
  ((place nil :type place)))

(def (function e) make-special-variable-place-inspector (name type)
  (make-instance 'place-inspector :place (make-special-variable-place name type)))

(def (macro e) make-lexical-variable-place-inspector (name type)
  `(make-instance 'place-inspector :place (make-lexical-variable-place ,name ,type)))

(def (function e) make-standard-object-slot-value-place-inspector (instance slot-name)
  (make-instance 'place-inspector :place (make-slot-value-place instance (find-slot (class-of instance) slot-name))))

(def render place-inspector ()
  (with-slots (edited content command-bar) -self-
    (if (and edited
             command-bar)
        (render-vertical-list (list command-bar content))
        (render content))))

(def method the-type-of ((self place-inspector))
  (place-type (place-of self)))

(def method refresh-component ((self place-inspector))
  (with-slots (place edited content command-bar) self
    (if content
        (unless (edited-p content)
          (revert-place-inspector-content self))
        (setf content (make-place-inspector-content self)))
    (setf command-bar (when (standard-object-inspector-place-p self)
                        (make-instance 'command-bar-component :commands (list (make-revert-place-command self)
                                                                              (make-set-place-to-nil-command self)
                                                                              (make-set-place-to-find-instance-command self)
                                                                              (make-set-place-to-new-instance-command self)))))))

(def function standard-object-inspector-place-p (component)
  (subtypep (first (ensure-list (find-inspector-component-type-for-type (place-type (place-of component))))) 'standard-object-inspector))

(def method (setf place-of) :after (new-value (self place-inspector))
  (setf (outdated-p self) #t))

(def function update-component-value-from-place (place component)
  (when (place-bound-p place)
    (bind ((value (value-at-place place)))
      (setf (component-value-of component)
            (if (prc::values-having-validity-p value)
                (prc::single-values-having-validity-value value)
                value)))))

(def function make-place-inspector-content (component)
  (bind ((place (place-of component)))
    (prog1-bind content
        (make-inspector-component (place-type place) :default-component-type 'reference-component)
      (update-component-value-from-place place content))))

(def method map-editable-child-components ((self place-inspector) function)
  (when (place-editable-p (place-of self))
    (awhen (content-of self)
      (funcall function it))))

(def function revert-place-inspector-content (place-inspector)
  (update-component-value-from-place (place-of place-inspector) (content-of place-inspector)))

(def method revert-editing :after ((place-inspector place-inspector))
  (revert-place-inspector-content place-inspector))

(def method store-editing :after ((place-inspector place-inspector))
  (setf (value-at-place (place-of place-inspector)) (place-component-value-of (content-of place-inspector))))

(def (function e) make-revert-place-command (place-inspector)
  (command (icon revert)
           (make-action (revert-place-inspector-content place-inspector))))

;;;;;;
;;; Place maker

(def component place-maker (place-component maker-component)
  ((the-type nil)))

(def method refresh-component ((self place-maker))
  (with-slots (the-type content command-bar) self
    (setf content (make-place-maker-content self)
          command-bar (when (standard-object-maker-place-p self)
                        (make-instance 'command-bar-component :commands (list (make-set-place-to-nil-command self)
                                                                              (make-set-place-to-find-instance-command self)
                                                                              (make-set-place-to-new-instance-command self)))))))

(def function standard-object-maker-place-p (component)
  (subtypep (first (ensure-list (find-maker-component-type-for-type (the-type-of component)))) 'standard-object-maker))

(def function make-place-maker-content (component)
  (if (standard-object-maker-place-p component)
      (make-inspector-component (the-type-of component) :default-component-type 'reference-component)
      (make-maker-component (the-type-of component))))

;;;;;;
;;; Place filter

(def component place-filter (place-component filter-component)
  ((the-type)))

(def method refresh-component ((self place-filter))
  (with-slots (the-type content command-bar) self
    (setf content (make-place-filter-content self)
          command-bar (when (standard-object-filter-place-p self)
                        (make-instance 'command-bar-component :commands (list (make-set-place-to-nil-command self)
                                                                              (make-set-place-to-find-instance-command self)))))))

(def function standard-object-filter-place-p (component)
  (subtypep (first (ensure-list (find-filter-component-type-for-type (the-type-of component)))) 'standard-object-filter))

(def function make-place-filter-content (component)
  (if (standard-object-filter-place-p component)
      (make-inspector-component (the-type-of component) :default-component-type 'reference-component)
      (make-filter-component (the-type-of component))))
