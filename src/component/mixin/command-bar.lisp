;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Command bar mixin

(def (component e) command-bar/mixin ()
  ((command-bar :type component))
  (:documentation "A COMPONENT with a COMMAND-BAR."))

(def refresh-component command-bar/mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (command-bar-of -self-) (make-command-bar -self- class prototype value))))

(def layered-method make-command-bar ((component command-bar/mixin) class prototype value)
  (make-instance 'command-bar/basic :commands (make-command-bar-commands component class prototype value)))

(def layered-method make-command-bar-commands ((component command-bar/mixin) class prototype value)
  nil)
