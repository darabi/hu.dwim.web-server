;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/inspector

(def layered-method make-alternatives ((component t/inspector) (class standard-class) (prototype standard-process) (value standard-process))
  (list* (make-instance 'standard-process/user-interface/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (call-next-method)))

;;;;;;
;;; standard-process/user-interface/inspector

(def (special-variable e) *process-component*)

(def (component e) standard-process/user-interface/inspector (inspector/style t/detail/inspector component-messages/widget content/mixin commands/mixin)
  ;; TODO: add support to command-bar
  ((answer-commands nil))
  (:documentation "A COMPONENT based on the continuation support provided by the hu.dwim.delico system."))

(def layered-method refresh-component ((self standard-process/user-interface/inspector))
  (bind (((:slots component-value) self))
    (unless (hu.dwim.util::continuation-of component-value)
      (roll-process self (component-dispatch-class self) (component-dispatch-prototype self) (component-value-of self)
                    (lambda (standard-process)
                      (bind ((walked-form (hu.dwim.walker::walk-form `(lambda () ,(hu.dwim.util::form-of standard-process)))))
                        (funcall (hu.dwim.delico::make-closure/cc walked-form))))))))

(def render-xhtml standard-process/user-interface/inspector
  (with-render-style/abstract (-self-)
    (render-component-messages-for -self-)
    (render-content-for -self-)
    (render-command-bar-for -self-)))

;; TODO: add support for computed commands in command-bar/widget
(def layered-method make-command-bar-commands ((component standard-process/user-interface/inspector) class prototype value)
  (dolist (command (answer-commands-of component))
    (setf (parent-component-of command) nil))
  (answer-commands-of component))

(def (function/cc e) call-component (component answer-commands)
  (let/cc k
    (setf (content-of *process-component*) component)
    (setf (answer-commands-of *process-component*) (ensure-list answer-commands))
    k))

(def (generic e) answer-component (component value)
  (:method ((component component) value)
    (answer-component (find-ancestor-component-with-type component 'standard-process/user-interface/inspector) value))

  (:method ((component standard-process/user-interface/inspector) value)
    (roll-process component (component-dispatch-class component) (component-dispatch-prototype component) (component-value-of component)
                  (lambda (standard-process)
                    (kall (hu.dwim.util::continuation-of standard-process) value)))))

(def (layered-function e) roll-process (component class prototype value thunk)
  (:method ((component standard-process/user-interface/inspector) class prototype value thunk)
    (bind ((*process-component* component))
      (funcall thunk value)))

  (:method ((component standard-process/user-interface/inspector) (class standard-class) (prototype standard-process) (value standard-process) thunk)
    (bind (((:slots answer-commands content) component)
           (values (multiple-value-list (call-next-layered-method)))
           (first-value (first values)))
      (if (hu.dwim.delico:continuationp first-value)
          (setf (hu.dwim.util::continuation-of value) first-value)
          (progn
            (when values
              (setf content (make-value-inspector first-value)))
            (setf answer-commands nil
                  (hu.dwim.util::continuation-of value) nil
                  (hu.dwim.util::result-of value) first-value)
            (add-component-information-message component "Process finished normally")))
      (mark-to-be-refreshed-component component)
      (values-list values))))

;;;;;;
;;; answer/widget
;;;
;;; TODO: support using plain command/widget and answer-component
;;; (make-answer-action) -> capture *process-component*

(def (icon e) answer-component)

(def (component e) answer/widget (command/widget)
  ((content (icon answer-component))
   (action nil)
   (return-value)))

(def constructor answer/widget
  (bind (((:slots action return-value) -self-))
    (unless action
      (setf action (make-action (answer-component -self- return-value))))))

(def (macro e) answer/widget ((&rest args &key &allow-other-keys) content &body forms)
  `(make-instance 'answer/widget ,@args
                  :content ,content
                  :return-value ,(when forms `(delay ,@forms))))
