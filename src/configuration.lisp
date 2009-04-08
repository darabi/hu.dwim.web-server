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
(def special-variable *quasi-quote-indentation-width* (unless *load-as-production-p* 2))

(def function make-str-transformation-pipeline ()
  (make-quasi-quoted-string-to-form-emitting-transformation-pipeline
   '*xml-stream*
   :binary *transform-quasi-quote-to-binary*
   :encoding +encoding+
   :with-inline-emitting *transform-quasi-quote-to-inline-emitting*))

(def function make-js-transformation-pipeline (&key embedded-in-xml inline-into-xml-attribute (inline-emitting *transform-quasi-quote-to-inline-emitting*))
  (make-quasi-quoted-js-to-form-emitting-transformation-pipeline
   (if embedded-in-xml
       '*xml-stream*
       '*js-stream*)
   :binary *transform-quasi-quote-to-binary*
   :encoding +encoding+
   :with-inline-emitting inline-emitting
   :indentation-width *quasi-quote-indentation-width*
   :escape-as-xml (and embedded-in-xml inline-into-xml-attribute)
   :output-prefix (cond
                    ((and embedded-in-xml
                          (not inline-into-xml-attribute))
                     (format nil "~%<script type=\"text/javascript\">~%// <![CDATA[~%")))
   :output-postfix (cond
                     ((and embedded-in-xml
                           (not inline-into-xml-attribute))
                      (format nil "~%// ]]>~%</script>~%"))
                     ((not embedded-in-xml) (format nil ";~%")))))

(def function make-xml-transformation-pipeline ()
  (make-quasi-quoted-xml-to-form-emitting-transformation-pipeline
   '*xml-stream*
   :binary *transform-quasi-quote-to-binary*
   :encoding +encoding+
   :with-inline-emitting *transform-quasi-quote-to-inline-emitting*
   :indentation-width *quasi-quote-indentation-width*
   ;; :emit-short-xml-element-form #f ; browsers simply suck, but for now let's just not disable it alltogether and use "" explicitly where it's supposed to be avoided
   :encoding +encoding+))

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

(defun setup-readtable ()
  (enable-quasi-quoted-list-to-list-emitting-form-syntax)
  (enable-sharp-boolean-syntax)
  (enable-feature-cond-syntax)
  (enable-lambda-with-bang-args-syntax :start-character #\[ :end-character #\])
  (enable-readtime-wrapper-syntax)
  (enable-sharpquote<>-syntax)
  (enable-quasi-quoted-string-syntax
   :transformation-pipeline (make-str-transformation-pipeline))
  ;; TODO these (= 1 *quasi-quote-lexical-depth*) should check for an xml lexically-parent syntax. what happens for ``js-inline() when used in a macro?
  (enable-quasi-quoted-js-syntax
   :transformation-pipeline (lambda ()
                              (if (= 1 *quasi-quote-lexical-depth*)
                                  (make-js-transformation-pipeline :embedded-in-xml nil)
                                  (make-js-transformation-pipeline :embedded-in-xml t)))
   :dispatched-quasi-quote-name 'js)
  (enable-quasi-quoted-js-syntax
   :transformation-pipeline (lambda ()
                              ;; the js-inline qq syntax is only in inline-emitting mode when used lexically-below another qq reader.
                              ;; this way it works as expected:
                              ;; (some-function `js-inline(+ 2 40))
                              ;; (defun some-function (on-click)
                              ;;   <div (:on-click ,on-click)>)
                              (if (= 1 *quasi-quote-lexical-depth*)
                                  (make-js-transformation-pipeline :embedded-in-xml t :inline-into-xml-attribute t :inline-emitting nil)
                                  (make-js-transformation-pipeline :embedded-in-xml t :inline-into-xml-attribute t)))
   :dispatched-quasi-quote-name 'js-inline)
  (enable-quasi-quoted-xml-syntax
   :transformation-pipeline (make-xml-transformation-pipeline)))

(register-readtable-for-swank
 '("HU.DWIM.WUI" "HU.DWIM.WUI-USER") 'setup-readtable)
