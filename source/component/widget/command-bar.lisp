;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; command-bar/abstract

(def (component e) command-bar/abstract (widget/abstract)
  ((commands :type components))
  (:documentation "A COMMAND-BAR with a SEQUENCE of COMMANDs."))

(def render-component :before command-bar/abstract
  (sort-command-bar-commands -self-))

(def (generic e) sort-command-bar-commands (component)
  (:documentation "Sorts the COMMANDs of COMMAND-BAR.")

  (:method :around ((self command-bar/abstract))
    (setf (commands-of self) (call-next-method)))

  (:method ((self command-bar/abstract))
    (sort (commands-of self) #'< :key #'command-position)))

;;;;;;
;;; command-bar/widget

(def (component e) command-bar/widget (command-bar/abstract widget/style)
  ())

(def (macro e) command-bar/widget ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'command-bar/widget ,@args :commands (optional-list ,@commands)))

(def render-component :in passive-layer :around command-bar/widget
  (values))

(def render-text command-bar/widget
  (foreach #'render-component (commands-of -self-)))

(def render-xhtml command-bar/widget
  (with-render-style/abstract (-self-)
    (iter (with commands = (commands-of -self-))
          (with length = (length commands))
          (with index = 0)
          (for command :in commands)
          (when (force (visible-component? command))
            <span (:class ,(element-style-class index length))
              ,(render-component command)>
            (incf index)))))

(def render-csv command-bar/widget
  (write-csv-separated-elements #\Space (commands-of -self-)))

(def generic find-command-bar (component)
  (:method ((self component))
    ;; FIXME: TODO: KLUDGE: command-bar is usually created from refresh and is not available after make-instance
    (ensure-refreshed self)
    (map-child-components self
                          (lambda (child)
                            (when (typep child 'command-bar/widget)
                              (return-from find-command-bar child)))))

  (:method ((self content/mixin))
    (or (call-next-method)
        (find-command-bar (content-of self)))))

(def (function e) push-command (command component)
  "Push a new COMMAND into the COMMAND-BAR of COMPONENT."
  (bind ((command-bar (find-command-bar component)))
    (assert command-bar nil "No command bar found, no place to push ~A in ~A" command component)
    (setf (commands-of command-bar) (cons command (commands-of command-bar)))))

(def function pop-command (command)
  "Pop the COMMAND from the containing COMMAND-BAR."
  (removef (commands-of (parent-component-of command)) command))
