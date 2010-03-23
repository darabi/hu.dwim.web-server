;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/lisp-form/inspector

(def (component e) t/lisp-form/inspector (inspector/style)
  ((source-objects :type list)
   (line-count :type integer)))

(def (macro e) t/lisp-form/inspector ((&rest args &key &allow-other-keys) &body form)
  `(make-instance 't/lisp-form/inspector ,@args :component-value ',(make-lisp-form-component-value* (the-only-element form))))

(def function make-lisp-form-component-value (form)
  (bind ((*print-case* :downcase)
         (*print-pretty* #t))
    (with-output-to-string (*standard-output*)
      (prin1 form))))

(def function make-lisp-form-component-value* (form)
  (if (stringp form)
      form
      (make-lisp-form-component-value form)))

;;;;;;
;;; Render lisp form

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
  (def render-component t/lisp-form/inspector
    (bind (((:read-only-slots line-count) -self-))
      (with-render-style/abstract (-self-)
        <pre (:class "gutter")
             ,(iter (for line-number :from 1 :to line-count)
                    <span (:class `str("line-number " ,(element-style-class (1- line-number) line-count)))
                          ,(format nil "~3,' ',D" line-number)>
                    ;; NOTE: this has to be separate to workaround an Opera browser issue related to
                    ;;       newline handling in pre elements
                    <span (:class "new-line") ,(format nil "~%")>)>
        <pre (:class "content")
             ,(foreach #'render-source-object (source-objects-of -self-))>)))}

(def layered-function render-source-object (instance))

;;;;;;
;;; t/lisp-form/invoker

(def (component e) t/lisp-form/invoker (t/lisp-form/inspector frame-unique-id/mixin commands/mixin)
  ((evaluation-mode :single :type (member :single :multiple))
   (result (empty/layout) :type component)))

(def (macro e) t/lisp-form/invoker ((&rest args &key &allow-other-keys) &body form)
  `(make-instance 't/lisp-form/invoker ,@args :component-value ',(make-lisp-form-component-value* (the-only-element form))))

(def render-xhtml t/lisp-form/invoker
  (with-render-style/abstract (-self-)
    (call-next-method)
    (render-command-bar-for -self-)
    <div (:class "result") ,(render-component (result-of -self-))>))

(def (icon e) evaluate-form)

(def layered-method make-command-bar-commands ((component t/lisp-form/invoker) class prototype value)
  (optional-list* (make-evaluate-form-command component class prototype value) (call-next-method)))

(def layered-method make-evaluate-form-command ((component t/lisp-form/invoker) class prototype value)
  (command/widget (:visible (delay (or (eq :multiple (evaluation-mode-of component))
                                       (empty-layout? (result-of component))))
                   :ajax (ajax-of component))
    (icon/widget evaluate-form)
    (make-component-action component
      (setf (result-of component)
            (handler-case (make-value-inspector (evaluate-form component class prototype value))
              (error (e)
                (make-value-inspector e)))))))

(def (layered-function e) evaluate-form (component class prototype value)
  (:method ((component t/lisp-form/invoker) class prototype value)
    (eval (read-from-string (component-value-of component)))))
