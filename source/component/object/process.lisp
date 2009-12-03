;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; TODO:
(eval-always
  (asdf:load-system :hu.dwim.def+hu.dwim.delico))

;;;;;;
;;; Process

;; TODO: try to kill this variable, if possible?!
(def (special-variable e) *process-component*)

(def (component e) t/process/inspector (inspector/style component-messages/widget content/mixin commands/mixin)
  ((closure/cc nil)
   (answer-continuation nil)
   ;; TODO: is this really?
   (command-bar (make-instance 'command-bar/widget :commands nil))))

(def (macro e) t/process/inspector ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 't/process/inspector ,@args :component-value '(progn ,@forms)))

(def refresh-component t/process/inspector
  (bind (((:slots closure/cc component-value) -self-))
    (setf closure/cc (hu.dwim.delico::make-closure/cc (hu.dwim.walker:walk-form `(lambda () ,component-value))))))

(def render-xhtml t/process/inspector
  ;; NOTE: answer-continuation and content are set during rendering
  (bind (((:slots answer-continuation content) -self-)
         ((:read-only-slots closure/cc) -self-))
    (unless content
      (setf answer-continuation
            (bind ((*process-component* -self-))
              (with-call/cc
                (funcall closure/cc)))))
    (unless (and content answer-continuation)
      ;; TODO: add a factory method or slot for this component
      (setf content "Process finished")
      (replace-answer-commands -self- nil))
    (with-render-style/abstract (-self-)
      (render-component-messages-for -self-)
      (render-content-for -self-)
      (render-command-bar-for -self-))))

(def function clear-process-component (component)
  (setf (content-of component) (empty/layout))
  (replace-answer-commands component nil))

(def function replace-answer-commands (component answer-commands)
  (bind ((command-bar (command-bar-of component)))
    (setf (commands-of command-bar)
          (append (remove-if (lambda (command)
                               (typep command 'answer/widget))
                             (commands-of command-bar))
                  answer-commands))))

(def (function/cc e) call-component (component &key answer-commands)
  (setf answer-commands (ensure-list answer-commands))
  (let/cc k
    (setf (content-of *process-component*) component)
    (replace-answer-commands *process-component* answer-commands)
    k))

(def (generic e) answer-component (component value)
  (:method ((component component) value)
    (answer-component (find-ancestor-component-with-type component 't/process/inspector) value))

  (:method ((component t/process/inspector) value)
    (bind ((*process-component* component))
      (setf (answer-continuation-of *process-component*)
            (kall (answer-continuation-of *process-component*) (force value))))))

;;;;;;
;;; answer/widget

(def (component e) answer/widget (command/widget)
  ((icon (icon answer :label "Answer")) ;; TODO localize
   (action nil)
   (value nil)))

(def constructor answer/widget ()
  (bind (((:slots icon action value) -self-))
    (unless action
      (setf action (make-action (answer-component -self- value))))))

(def (macro e) answer/widget ((&rest args &key &allow-other-keys) content &body forms)
  `(make-instance 'answer/widget ,@args
                  :content ,content
                  :value ,(when forms `(delay ,@forms))))
