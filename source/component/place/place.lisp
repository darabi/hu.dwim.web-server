;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place component

(def (component e) place-component (content/mixin)
  ((command-bar nil :type component)))

(def generic place-component-value-of (component)
  (:method ((self component))
    (component-value-of self)))

(def generic (setf place-component-value-of) (new-value component)
  (:method (new-value (self component))
    (setf (component-value-of self) new-value)))

(def generic make-place-component-content (place-component))

(def generic make-place-component-command-bar (place-component)
  (:method ((self place-component))
    nil))

(def refresh-component place-component
  (bind (((:slots content command-bar) -self-))
    (setf content (make-place-component-content -self-)
          command-bar (make-place-component-command-bar -self-))))

(def render-xhtml place-component
  (bind (((:slots content command-bar) -self-))
    (if command-bar
        (render-vertical-list (list command-bar content))
        (call-next-method))))

(def (function e) make-set-place-to-nil-command (place-component)
  (command (:visible (delay (bind ((content (content-of place-component)))
                              (and (typep content 'inspector/abstract)
                                   (not (null (component-value-of content)))))))
    (icon set-to-nil)
    (make-action
      (setf (component-value-of (content-of place-component)) nil))))

(def (function e) make-set-place-to-unbound-command (place-component)
  (command (:visible (delay (not (typep (content-of place-component) 'unbound-component))))
    (icon set-to-unbound)
    (make-action
      (when-bind command (find-command (content-of place-component) 'back)
        (execute-command command))
      (setf (content-of place-component) (make-instance 'unbound-component)))))

(def (function e) make-set-place-to-new-instance-command (place-component)
  (make-replace-and-push-back-command (delay (content-of place-component))
                                      (make-maker (the-type-of place-component))
                                      (list :content (icon new) )
                                      (list :content (icon back))))

(def layer* set-place-to-find-instance-layer ()
  ;; TODO: KLUDGE: FIXME: make this a special slot (this way it is not thread safe)
  ((place-component :type component)))

(def layered-method make-context-menu-items :in-layer set-place-to-find-instance-layer :around
     ((component standard-object-row-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
     ;; TODO: check for being the row of the result component of the filter in the place component
  (bind ((place-component
          (parent-component-of (find-ancestor-component-with-type component 'standard-object-filter))
          #+nil
          (place-component-of (current-layer))))
    (list* (command ()
             (icon select)
             (make-action
               (execute-command (find-command (find-ancestor-component-with-type component 'standard-object-filter) 'back))
               (setf (content-of place-component) (make-viewer (instance-of component)))))
           (call-next-method))))

(def (function e) make-set-place-to-find-instance-command (place-component)
  (make-replace-and-push-back-command (delay (content-of place-component))
                                      (with-active-layers ((set-place-to-find-instance-layer :place-component place-component))
                                        (make-filter (the-type-of place-component)))
                                      (list :content (icon find))
                                      (list :content (icon back))))
