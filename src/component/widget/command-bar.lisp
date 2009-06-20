;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Command bar

(def (component e) command-bar ()
  ()
  (:documentation "A COMMAND-BAR base class."))

(def (macro e) command-bar ((&rest args &key remote-setup &allow-other-keys) &body commands)
  (remove-from-plistf args :remote-setup)
  (if remote-setup
      `(command-bar/full ,args ,@commands)
      `(command-bar/basic ,args ,@commands)))

(def render-component :in passive-layer :around command-bar
  (values))

;;;;;;
;;; Command bar abstract

(def (component e) command-bar/abstract (command-bar component/abstract)
  ((commands :type components))
  (:documentation "A COMMAND-BAR with a list of COMMANDs."))

(def render-component :before command-bar/abstract
  (setf (commands-of -self-) (sort-command-bar-commands -self-)))

(def (generic e) sort-command-bar-commands (component)
  (:documentation "Sorts the COMMANDs of COMMAND-BAR")

  (:method ((self command-bar/abstract))
    (sort (commands-of self) #'< :key #'command-bar-command-position)))

(def (generic e) command-bar-command-position (component)
  (:method ((self number))
    most-positive-fixnum)

  (:method ((self string))
    most-positive-fixnum)

  (:method ((self component))
    most-positive-fixnum)

  (:method ((self icon/basic))
    ;; TODO: can't we make it faster/better (what about a generic method or something?)
    (or (position (name-of self)
                  ;; TODO: this name thingie is fragile
                  '(answer back focus-out open-in-new-frame focus-in collapse collapse-all expand-all refresh edit save cancel store revert new delete)
                  :test #'eq)
        most-positive-fixnum))

  (:method ((self command/basic))
    (command-bar-command-position (content-of self))))

;;;;;;
;;; Command bar basic

(def (component e) command-bar/basic (command-bar/abstract component/basic)
  ())

(def (macro e) command-bar/basic ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'command-bar/basic ,@args :commands (optional-list ,@commands)))

(def render-xhtml command-bar/basic
  <div (:class "command-bar")
    ,(foreach #'render-component (commands-of -self-))>)

(def render-csv command-bar/basic
  (write-csv-separated-elements #\Space (commands-of -self-)))

(def generic find-command-bar (component)
  (:method ((component component))
    (map-child-components component
                          (lambda (child)
                            (when (typep child 'command-bar/basic)
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

;;;;;;
;;; Command bar full

(def (component e) command-bar/full (command-bar/basic component/full)
  ())

(def (macro e) command-bar/full ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'command-bar/full ,@args :commands (optional-list ,@commands)))

(def render-xhtml command-bar/full
  (with-render-style/abstract (-self-)
    (foreach #'render-component (commands-of -self-))))
