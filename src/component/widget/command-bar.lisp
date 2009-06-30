;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Command bar abstract

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
    (sort (commands-of self) #'< :key #'command-bar-command-position)))

(def (generic e) command-bar-command-position (component)
  (:method ((self number))
    most-positive-fixnum)

  (:method ((self string))
    most-positive-fixnum)

  (:method ((self component))
    most-positive-fixnum)

  (:method ((self icon/widget))
    ;; TODO: can't we make it faster/better (what about a generic method or something?)
    (or (position (name-of self)
                  ;; TODO: this name thingie is fragile
                  '(answer back focus-out open-in-new-frame focus-in collapse collapse-all expand-all refresh edit save cancel store revert new delete))
        most-positive-fixnum))

  (:method ((self command/widget))
    (command-bar-command-position (content-of self))))

;;;;;;
;;; Command bar widget

(def (component e) command-bar/widget (command-bar/abstract widget/basic)
  ())

(def (macro e) command-bar/widget ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'command-bar/widget ,@args :commands (optional-list ,@commands)))

(def render-xhtml command-bar/widget
  <div (:class "command-bar")
    ,(foreach #'render-component (commands-of -self-))>)

(def render-csv command-bar/widget
  (write-csv-separated-elements #\Space (commands-of -self-)))

(def generic find-command-bar (component)
  (:method ((self component))
    (map-child-components self
                          (lambda (child)
                            (when (typep child 'command-bar/widget)
                              (return-from find-command-bar child))))))

(def (function e) push-command (command component)
  "Push a new COMMAND into the COMMAND-BAR of COMPONENT."
  ;; FIXME: TODO: KLUDGE: command-bar is usually created from refresh and is not available after make-instance
  (ensure-refreshed component)
  (bind ((command-bar (find-command-bar component)))
    (assert command-bar nil "No command bar found, no place to push ~A in ~A" command component)
    (setf (commands-of command-bar) (cons command (commands-of command-bar)))))

(def function pop-command (command)
  "Pop the COMMAND from the containing COMMAND-BAR."
  (removef (commands-of (parent-component-of command)) command))
