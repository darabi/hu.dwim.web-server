;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; closure-cc/inspector

(def (component e) closure-cc/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null hu.dwim.delico::closure/cc) closure-cc/inspector)

(def layered-method make-alternatives ((component closure-cc/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'closure-cc/user-interface/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; t/user-interface/inspector

(def (special-variable e) *process-component*)

(def (component e) t/user-interface/inspector (inspector/style component-messages/widget content/mixin commands/mixin)
  ((answer-continuation nil)
   (answer-commands nil)))

(def layered-method make-command-bar-commands ((component t/user-interface/inspector) class prototype value)
  (dolist (command (answer-commands-of component))
    (setf (parent-component-of command) nil))
  (append (answer-commands-of component) (call-next-method)))

(def (function/cc e) call-component (component &key answer-commands)
  (setf answer-commands (ensure-list answer-commands))
  (let/cc k
    (setf (content-of *process-component*) component)
    (setf (answer-commands-of *process-component*) answer-commands)
    k))

(def (generic e) answer-component (component value)
  (:method ((component component) value)
    (answer-component (find-ancestor-component-with-type component 't/user-interface/inspector) value))

  (:method ((component t/user-interface/inspector) value)
    (bind ((*process-component* component))
      (setf (answer-continuation-of *process-component*)
            (kall (answer-continuation-of *process-component*) (force value))))))

(def function finish-process-component (component)
  (setf (content-of component) (empty/layout)
        (answer-commands-of *process-component*) nil))

;;;;;;
;;; answer/widget

(def (component e) answer/widget (command/widget)
  ((action nil)
   (return-value)))

(def constructor answer/widget ()
  (bind (((:slots action return-value) -self-))
    (unless action
      (setf action (make-action (answer-component -self- return-value))))))

(def (macro e) answer/widget ((&rest args &key &allow-other-keys) content &body forms)
  `(make-instance 'answer/widget ,@args
                  :content ,content
                  :return-value ,(when forms `(delay ,@forms))))

;;;;;;
;;; closure-cc/user-interface/inspector

(def (component e) closure-cc/user-interface/inspector (t/user-interface/inspector)
  ())

(def (macro e) closure-cc/user-interface/inspector ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'closure-cc/user-interface/inspector ,@args
                  :component-value (hu.dwim.delico::make-closure/cc (hu.dwim.walker:walk-form `(lambda () ,',@forms)))))

(def render-xhtml closure-cc/user-interface/inspector
  ;; NOTE: answer-continuation and content are set during rendering
  (bind (((:slots answer-continuation content) -self-))
    (unless content
      (setf answer-continuation
            (bind ((*process-component* -self-))
              (with-call/cc
                (funcall (component-value-of -self-))))))
    (unless (and content answer-continuation)
      (finish-process-component -self-))
    (with-render-style/abstract (-self-)
      (render-component-messages-for -self-)
      (render-content-for -self-)
      (render-command-bar-for -self-))))
