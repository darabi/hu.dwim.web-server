;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def constant +default-encoding+ :utf-8)

(def special-variable *transform-quasi-quote-to-inline-emitting* t)
(def special-variable *transform-quasi-quote-to-binary* t)
(def special-variable *quasi-quote-indentation-width* (unless *load-as-production?* 2))

(def function make-str-transformation-pipeline ()
  (make-quasi-quoted-string-to-form-emitting-transformation-pipeline
   '*xml-stream*
   :binary *transform-quasi-quote-to-binary*
   :encoding +default-encoding+
   :with-inline-emitting *transform-quasi-quote-to-inline-emitting*))

(def function make-js-transformation-pipeline (&key embedded-in-xml inline-into-xml-attribute (inline-emitting *transform-quasi-quote-to-inline-emitting*)
                                                    (output-postfix nil output-postfix-provided?) (output-prefix nil output-prefix-provided?))
  (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
   (if embedded-in-xml
       '*xml-stream*
       '*js-stream*)
   :binary *transform-quasi-quote-to-binary*
   :encoding +default-encoding+
   :with-inline-emitting inline-emitting
   :indentation-width *quasi-quote-indentation-width*
   :escape-as-xml (and embedded-in-xml inline-into-xml-attribute)
   :output-prefix (cond
                    (output-prefix-provided? output-prefix)
                    ((and embedded-in-xml
                          (not inline-into-xml-attribute))
                     (format nil "~%<script type=\"text/javascript\">~%// <![CDATA[~%")))
   :output-postfix (cond
                     (output-postfix-provided? output-postfix)
                     ((and embedded-in-xml
                           (not inline-into-xml-attribute))
                      (format nil "~%// ]]>~%</script>~%"))
                     ((not embedded-in-xml) (format nil ";~%")))))

(def function make-xml-transformation-pipeline ()
  (make-quasi-quoted-xml-to-form-emitting-transformation-pipeline
   '*xml-stream*
   :binary *transform-quasi-quote-to-binary*
   :encoding +default-encoding+
   :with-inline-emitting *transform-quasi-quote-to-inline-emitting*
   :indentation-width *quasi-quote-indentation-width*
   ;; :emit-short-xml-element-form nil ; browsers simply suck, but for now let's just not disable it alltogether and use "" explicitly where it's supposed to be avoided
   :encoding +default-encoding+))

(define-syntax js-sharpquote ()
  (set-dispatch-macro-character
   #\# #\"
   (lambda (s c1 c2)
     (declare (ignore c2))
     (unread-char c1 s)
     (let ((key (read s)))
       `(|wui.i18n.localize| ,key)))))

(define-syntax sharpquote<> ()
  "Enable quote reader for the rest of the file (being loaded or compiled).
#\"my i18n text\" parts will be replaced by a LOOKUP-RESOURCE call for the string."
  ;; the reader itself needs the <> syntax, so it's in utils.lisp
  (set-dispatch-macro-character #\# #\" (lambda (stream c1 c2)
                                          (localized-string-reader stream c1 c2))))

(def function setup-readtable ()
  (enable-quasi-quoted-list-to-list-emitting-form-syntax)
  (enable-sharp-boolean-syntax)
  (enable-feature-cond-syntax)
  (enable-lambda-with-bang-args-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-sharpquote<>-syntax)
  (enable-quasi-quoted-string-syntax :transformation-pipeline (make-str-transformation-pipeline))
  ;; TODO these (= 1 *quasi-quote-lexical-depth*) should check for an xml lexically-parent syntax. what happens for ``js-inline() when used in a macro?
  (bind ((toplevel-pipeline (make-js-transformation-pipeline :embedded-in-xml nil))
         (nested-pipeline (make-js-transformation-pipeline :embedded-in-xml t)))
    (enable-quasi-quoted-js-syntax
     :transformation-pipeline nested-pipeline
     :dispatched-quasi-quote-name 'js-xml)
    (enable-quasi-quoted-js-syntax
     :transformation-pipeline toplevel-pipeline
     :dispatched-quasi-quote-name 'js)
    (enable-quasi-quoted-js-syntax
     :transformation-pipeline (list (make-instance 'quasi-quoted-js-to-quasi-quoted-js-building-forms
                                                   :result-quasi-quote-pipeline nested-pipeline))
     :dispatched-quasi-quote-name 'js-ast))
  (enable-quasi-quoted-js-syntax
   :transformation-pipeline (make-js-transformation-pipeline :embedded-in-xml nil :output-postfix nil :output-prefix nil)
   :toplevel-reader-wrapper (lambda (reader)
                              (named-lambda js-piece/toplevel-wrapper (&rest args)
                                (bind (((:values result js?) (apply reader args)))
                                  (if js?
                                      (if *transform-quasi-quote-to-binary*
                                          `(make-instance 'binary-quasi-quote :body (with-output-to-sequence (*js-stream*)
                                                                                      ,result))
                                          `(make-instance 'string-quasi-quote :body (with-output-to-string (*js-stream*)
                                                                                      ,result)))
                                      result))))
   :dispatched-quasi-quote-name 'js-piece)
  (enable-quasi-quoted-js-syntax
   :transformation-pipeline (bind ((toplevel-pipeline (make-js-transformation-pipeline :embedded-in-xml t :inline-into-xml-attribute t :inline-emitting nil))
                                   (nested-pipeline (make-js-transformation-pipeline :embedded-in-xml t :inline-into-xml-attribute t)))
                              (lambda ()
                                ;; the js-inline qq syntax is only in inline-emitting mode when used lexically-below another qq reader.
                                ;; this way it works as expected:
                                ;; (some-function `js-inline(+ 2 40))
                                ;; (def function some-function (on-click)
                                ;;   <div (:on-click ,on-click)>)
                                (if (= 1 *quasi-quote-lexical-depth*)
                                    toplevel-pipeline
                                    nested-pipeline)))
   :dispatched-quasi-quote-name 'js-inline)
  (enable-quasi-quoted-xml-syntax
   :transformation-pipeline (make-xml-transformation-pipeline)))

(register-readtable-for-swank
 '(:hu.dwim.wui) 'setup-readtable)

#+nil
(def (macro e) transform-js (&body body)
  (if *transform-quasi-quote-to-binary*
      `(make-instance 'binary-quasi-quote :body (with-output-to-sequence (*js-stream*)
                                                  ,@body))
      `(make-instance 'string-quasi-quote :body (with-output-to-string (*js-stream*)
                                                  ,@body))))

(def function %transform-xml (js-quoted? body)
  (bind ((content (if *transform-quasi-quote-to-binary*
                      `(babel:octets-to-string
                        (with-output-to-sequence (*xml-stream*)
                          ,@body)
                        :encoding +default-encoding+)
                      `(with-output-to-string (*xml-stream*)
                         ,@body))))
    (once-only (content)
      `(progn
         ,(when js-quoted?
            `(setf ,content (concatenate 'string "\'" (escape-as-js-string ,content) "\'")))
         (make-instance 'string-quasi-quote
                        :body ,content
                        :transformation-pipeline ',(if *transform-quasi-quote-to-binary*
                                                       (list
                                                        (make-instance 'quasi-quoted-string-to-quasi-quoted-binary
                                                                       :encoding +default-encoding+)
                                                        (make-instance 'quasi-quoted-binary-to-binary-emitting-form
                                                                       :stream-variable-name '*xml-stream*
                                                                       :with-inline-emitting *transform-quasi-quote-to-inline-emitting*))
                                                       (list
                                                        (make-instance 'quasi-quoted-string-to-string-emitting-form
                                                                       :stream-variable-name '*xml-stream*
                                                                       :with-inline-emitting *transform-quasi-quote-to-inline-emitting*))))))))

(def (macro e) transform-xml (&body body)
  (%transform-xml nil body))

(def (macro e) transform-xml/js-quoted (&body body)
  (%transform-xml t body))

(def (function e) with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace ()
    "Unconditionally turns off XML indent to keep original whitespaces in the resulting XML."
    (with-quasi-quoted-xml-to-binary-emitting-form-syntax '*xml-stream* :with-inline-emitting #t))
