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
;;; shell-script/alternator/inspector

(def (component e) shell-script/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null shell-script) shell-script/alternator/inspector)

(def layered-method make-alternatives ((component shell-script/alternator/inspector) (class standard-class) (prototype shell-script) (value shell-script))
  (list* (make-instance 'shell-script/text/inspector :component-value value) (call-next-layered-method)))

;;;;;;
;;; string/shell-script/inspector

(def (component e) shell-script/text/inspector (t/text/inspector)
  ())

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
  (def render-xhtml shell-script/text/inspector
    (bind (((:read-only-slots component-value) -self-)
           (contents (contents-of component-value)))
      (with-render-style/component (-self- :element-name "ol")
        (iter (with length = (length contents))
              (for index :from 0)
              (for line :in contents)
              ;; NOTE: do not put new lines in here, because it is preformatted
              <li <pre (:class `str("command " ,(element-style-class index length))) ,(render-component line)>>))))}

(def render-text shell-script/text/inspector
  (iter (for content :in (contents-of -self-))
        (write-text-line-begin)
        (write-string "$ " *text-stream*)
        (render-component content)
        (write-text-line-separator)))

(def render-sh shell-script/text/inspector
  (iter (for content :in (contents-of -self-))
        (render-component content)
        (write-text-line-separator)))

(def render-ods shell-script/text/inspector
  <table:table-row
    <table:table-cell
     ,(iter (for content :in (contents-of -self-))
            (render-component content))>>)

(def render-odt shell-script/text/inspector
  <text:p (text:style-name "Code")
    ,(iter (for content :in (contents-of -self-))
           <text:line-break>
           (render-component content))>)

(def layered-method write-text-line-begin :in sh-layer  ()
  (write-string "# " *text-stream*))
