;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; command-bar/mixin

(def (component e) command-bar/mixin ()
  ((command-bar nil :type component))
  (:documentation "A COMPONENT with a COMMAND-BAR."))

(def refresh-component command-bar/mixin
  (bind (((:slots command-bar) -self-))
    (bind ((class (component-dispatch-class -self-))
           (prototype (component-dispatch-prototype -self-))
           (value (component-value-of -self-)))
      (setf command-bar (make-command-bar -self- class prototype value)))))

(def (function e) render-command-bar-for (component)
  (awhen (command-bar-of component)
    (render-component it)))

(def layered-method make-command-bar ((component command-bar/mixin) class prototype value)
  (make-instance 'command-bar/widget :commands (make-command-bar-commands component class prototype value)))
