;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place component

(def component place-component (editable-component)
  ((place)
   (content :type component)
   (command-bar :type component))
  (:documentation "Place component is resposible for being able to edit any value that is valid according to the type."))

(def constructor place-component ()
  (with-slots (edited content command-bar) -self-
    (setf content (make-place-component-content -self-)
          command-bar (make-instance 'command-bar-component :commands (append (list (make-refresh-command -self-)
                                                                                    #+nil
                                                                                    (make-makunbound-command -self-))
                                                                              (make-editing-commands -self-))))))

(def render place-component ()
  (with-slots (edited content command-bar) -self-
    (render content)
    #+nil
    (render-vertical-list (list content command-bar))))

(def function make-place-component-content (component)
  (bind ((place (place-of component))
         (type (place-type place))
         (content (make-inspector-component type :default-component-type 'reference-component)))
    (when (place-bound-p place)
      (setf (component-value-of content) (value-at-place place)))
    content))

(def (function e) make-special-variable-place-component (name type)
  (make-instance 'place-component :place (make-special-variable-place name type)))

(def (macro e) make-lexical-variable-place-component (name type)
  `(make-instance 'place-component :place (make-lexical-variable-place ,name ,type)))

(def (function e) make-standard-object-slot-value-place-component (instance slot-name)
  (make-instance 'place-component :place (make-slot-value-place instance (find-slot (class-of instance) slot-name))))

(def function revert-place-component-content (place-component)
  (setf (content-of place-component) (make-place-component-content place-component)))

(def method refresh-component :after ((place-component place-component))
  (unless (edited-p place-component)
    (revert-place-component-content place-component)))

(def method revert-editing :after ((place-component place-component))
  (revert-place-component-content place-component))

(def method store-editing :after ((place-component place-component))
  (setf (value-at-place (place-of place-component)) (component-value-of (content-of place-component))))

(def (function e) make-makunbound-command (place-component)
  (make-replace-command (delay (content-of place-component))
                        (delay (make-place-component-content place-component))
                        :icon (make-icon-component 'makunbound :label "Makunbound")
                        :visible (delay (not (typep (content-of place-component) 'unbound-component)))))

(def (function e) make-revert-command (place-component)
  (make-instance 'command-component
                 :icon (make-icon-component 'revert :label "Revert")
                 :action (make-action (revert-place-component-content place-component))))
