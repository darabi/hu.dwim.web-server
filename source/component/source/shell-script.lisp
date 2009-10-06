;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; shell-script

(def (class* e) shell-script ()
  ((contents :type list)))

(def (macro e) shell-script ((&rest args &key &allow-other-keys) &body lines)
  `(make-instance 'shell-script ,@args :contents (list ,@lines)))

;;;;;;
;;; shell-script/inspector

(def (component e) shell-script/inspector (t/inspector)
  ())

(def (macro e) shell-script/inspector ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'shell-script/inspector ,@args :contents (list ,@contents)))

(def layered-method find-inspector-type-for-prototype ((prototype shell-script))
  'shell-script/inspector)

(def layered-method make-alternatives ((component shell-script/inspector) class prototype (value shell-script))
  (list* (delay-alternative-component-with-initargs 'shell-script/text/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; string/shell-script/inspector

(def (component e) shell-script/text/inspector (t/text/inspector)
  ())

(def (macro e) shell-script/text/inspector ((&rest args &key &allow-other-keys) &body shell-script)
  `(make-instance 'shell-script/text/inspector ,@args :component-value ,(the-only-element shell-script)))

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
  (def render-xhtml shell-script/text/inspector
    (bind (((:read-only-slots component-value) -self-))
      (with-render-style/abstract (-self-)
        <pre (:class "gutter")
             ,(iter (with line-count = (length (contents-of component-value)))
                    (for line-number :from 0)
                    (repeat line-count)
                    <span (:class `str("line-number " ,(element-style-class line-number line-count))) ,(format nil "~3,' ',D" (1+ line-number))>
                    <span (:class "prompt") ,(format nil "$ ~%")>)>
        <pre (:class "content")
             ,(iter (for line :in (contents-of component-value))
                    (unless (first-iteration-p)
                      (write-char #\NewLine *xml-stream*))
                    <span (:class "command") ,(render-component line)>)>)))}

(def render-text shell-script/text/inspector
  (iter (for content :in (contents-of -self-))
        (write-text-line-begin)
        (write-string "$ " *text-stream*)
        (render-component content)
        (write-text-line-separator)))

(def render-shell-script shell-script/text/inspector
  (iter (for content :in (contents-of -self-))
        (render-component content)
        (write-text-line-separator)))

(def layered-method write-text-line-begin :in shell-script-layer  ()
  (write-string "# " *text-stream*))

;;;;;;
;;; Render shell-script

(def (icon e) export-shell-script)

(def (layered-function e) export-shell-script (component)
  (:documentation "Export COMPONENT into shell script.")

  (:method (component)
    (render-shell-script component)))

(def layered-method make-export-command ((format (eql :sh)) component class prototype value)
  (command/widget (:delayed-content #t
                   :path (export-file-name format component))
    (icon export-shell-script)
    (make-component-action component
      (with-output-to-export-stream (*text-stream* :content-type +text-mime-type+ :external-format :utf-8)
        (export-shell-script component)))))
