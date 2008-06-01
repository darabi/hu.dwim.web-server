;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def macro debug-only (&body body)
  (if *load-as-production-p*
      (values)
      `(progn
         ,@body)))

(def macro debug-only* (&body body)
  `(unless *load-as-production-p*
     ,@body))

(def macro production-only (&body body)
  (if *load-as-production-p*
      `(progn
         ,@body)
      (values)))

(def macro production-only* (&body body)
  `(if *load-as-production-p*
       (progn
         ,@body)
       (values)))

(defun transform-function-definer-options (options)
  (if *load-as-production-p*
      options
      (remove-from-plist options :inline :optimize)))

(def constant +encoding+ :utf-8)

(def special-variable *transform-quasi-quote-to-inline-emitting* t)
(def special-variable *transform-quasi-quote-to-binary* t)
(def special-variable *quasi-quote-indentation-width* nil)

(def function make-str-transformation-pipeline ()
  (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
   '*html-stream*
   :binary *transform-quasi-quote-to-binary*
   :encoding +encoding+
   :with-inline-emitting *transform-quasi-quote-to-inline-emitting*))

(def function make-js-transformation-pipeline (embedded-in-xml? &optional inline?)
  (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
   (if embedded-in-xml?
       '*html-stream*
       '*js-stream*)
   :binary *transform-quasi-quote-to-binary*
   :encoding +encoding+
   :with-inline-emitting *transform-quasi-quote-to-inline-emitting*
   :indentation-width *quasi-quote-indentation-width*
   :output-prefix (cond
                    ;; TODO and it must also be xml escaped... think through how this should work in qq.
                    ;; should `str() inside <> automatically be escaped? how could you insert unescaped
                    ;; then?
                    (inline?          (format nil "javascript: "))
                    (embedded-in-xml? (format nil "~%<script>~%// <![CDATA[~%")))
   :output-postfix (when (and embedded-in-xml?
                              (not inline?))
                     (format nil "~%// ]]>~%</script>~%"))))

(def function make-xml-transformation-pipeline ()
  (make-quasi-quoted-xm-to-form-emitting-transformation-pipeline
   '*html-stream*
   :binary *transform-quasi-quote-to-binary*
   :encoding +encoding+
   :with-inline-emitting *transform-quasi-quote-to-inline-emitting*
   :indentation-width *quasi-quote-indentation-width*
   :encoding +encoding+))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-lambda-with-bang-args-syntax :start-character #\[ :end-character #\])
  (enable-readtime-wrapper-syntax)
  (enable-sharpquote<>-syntax)
  (enable-quasi-quoted-string-syntax
   :transformation-pipeline (make-str-transformation-pipeline))
  (enable-quasi-quoted-js-syntax
   :transformation-pipeline (make-js-transformation-pipeline nil)
   :nested-transformation-pipeline (make-js-transformation-pipeline t))
  (enable-quasi-quoted-js-syntax
   :transformation-pipeline (make-js-transformation-pipeline t t)
   :dispatched-quasi-quote-name 'js-inline)
  (enable-quasi-quoted-xml-syntax
   :transformation-pipeline (make-xml-transformation-pipeline)))

(register-readtable-for-swank
 '("HU.DWIM.WUI" "HU.DWIM.WUI-USER") 'setup-readtable)
