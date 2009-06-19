;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Command bar mixin

(def (component e) command-bar/mixin ()
  ((command-bar :type component))
  (:documentation "A component with a command bar."))

(def refresh-component command-bar/mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (command-bar-of -self-) (make-command-bar -self- class prototype value))))

(def layered-method make-command-bar ((component command-bar/mixin) class prototype value)
  (make-instance 'command-bar/basic :commands (make-command-bar-commands component class prototype value)))

(def layered-method make-command-bar-commands ((component command-bar/mixin) class prototype value)
  nil)

;;;;;;
;;; Command bar basic

(def (component e) command-bar/basic ()
  ((commands :type components)))

(def (macro e) command-bar (&body commands)
  `(make-instance 'command-bar/basic :commands (optional-list ,@commands)))

(def render-xhtml command-bar/basic
  (bind (((:read-only-slots parent-component commands) -self-)
         (sorted-commands (sort-commands parent-component commands)))
    (setf (commands-of -self-) sorted-commands)
    (render-horizontal-list sorted-commands :style-class "command-bar")))

(def render-csv command-bar/basic
  (write-csv-separated-elements #\Space (commands-of -self-)))

(def render-component :in passive-components-layer command-bar/basic
  (values))

(def generic find-command-bar (component)
  (:method ((component component))
    (map-child-components component
                          (lambda (child)
                            (when (typep child 'command-bar/basic)
                              (return-from find-command-bar child))))))

(def generic sort-commands (component commands)
  (:method ((component component) commands)
    (sort commands #'<
          :key (lambda (command)
                 ;; TODO: can't we make it faster/better (what about a generic method or something?)
                 (bind ((content (content-of command)))
                   (or (when (typep content 'icon/basic)
                         (position (name-of (content-of command))
                                   '(answer back focus-out open-in-new-frame focus-in collapse collapse-all expand-all refresh edit save cancel store revert new delete)
                                   :test #'eq))
                       most-positive-fixnum))))))

(def (function e) push-command (command component)
  "Push a new COMMAND into the COMMAND-BAR of COMPONENT."
  ;; FIXME: TODO: KLUDGE: command-bar is usually created from refresh and is not available after make-instance
  (ensure-refreshed component)
  (bind ((command-bar (find-command-bar component)))
    (assert command-bar nil "No command bar found, no place to push ~A in ~A" command component)
    (setf (commands-of command-bar)
          (sort-commands component (cons command (commands-of command-bar))))))

(def function pop-command (command)
  "Pop the COMMAND from the containing COMMAND-BAR."
  (removef (commands-of (parent-component-of command)) command))
