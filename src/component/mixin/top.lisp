;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; top/abstract

(def (component e) top/abstract (content/mixin)
  ()
  (:documentation "A COMPONENT that is related to the FOCUS command."))

(def (function e) find-top-component (component)
  (find-ancestor-component-with-type component 'top/abstract))

(def (function e) top-component? (component)
  (eq component (find-top-component component)))

(def (function e) find-top-component-content (component)
  (awhen (find-top-component component)
    (content-of it)))

(def (function e) top-component-content? (component)
  (eq component (find-top-component-content component)))

(def layered-method make-move-commands ((component top/abstract) class prototype value)
  (optional-list* (make-focus-command component class prototype value) (call-next-method)))
