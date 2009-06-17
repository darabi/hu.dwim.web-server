;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Top abstract

(def (component ea) top/abstract (content/mixin)
  ()
  (:documentation "A component that is related to the FOCUS command."))

(def (function e) find-top-component (component)
  (find-ancestor-component-with-type component 'top/abstract))

(def (function e) top-component? (component)
  (eq component (find-top-component component)))

(def (function e) find-top-component-content (component)
  (awhen (find-top-component component)
    (content-of it)))

(def (function e) top-component-content? (component)
  (eq component (find-top-component-content component)))

(def icon focus-in)

(def icon focus-out)

(def (layered-function e) make-focus-command (component classs prototype value)
  (:documentation "The FOCUS command replaces the top level COMPONENT usually found under the FRAME with the given REPLACEMENT-COMPONENT")

  (:method ((component component) (class standard-class) (prototype standard-object) value)
    (bind ((original-component (delay (find-top-component-content component))))
      (make-replace-and-push-back-command original-component component
                                          (list :content (icon focus-in) :visible (delay (not (top-component-content? component))))
                                          (list :content (icon focus-out))))))

;;;;;;
;;; Top basic

(def (component ea) top/basic (top/abstract style/abstract user-messages/mixin)
  ())

(def (macro e) top (() &body content)
  `(make-instance 'top/basic :content ,(the-only-element content)))

(def render-xhtml top/basic
  (with-render-style/abstract (-self-)
    (render-user-messages -self-)
    (call-next-method)))

;;;;;;
;;; Localization

(def resources hu
  (icon-label.focus-in "Fókuszálás")
  (icon-tooltip.focus-in "Fókuszálás az objektumra")

  (icon-label.focus-out "Vissza")
  (icon-tooltip.focus-out "Fókuszálás megszüntetése"))

(def resources en
  (icon-label.focus-in "Focus")
  (icon-tooltip.focus-in "Focus on the object")

  (icon-label.focus-out "Back")
  (icon-tooltip.focus-out "Undo focus"))
