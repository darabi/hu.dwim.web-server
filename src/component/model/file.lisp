;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; source-file/inspector

(def (component e) source-file/inspector (t/inspector)
  ())

(def (macro e) source-file/inspector ((&rest args &key &allow-other-keys) &body file)
  `(make-instance 'source-file/inspector ,@args :component-value ,(the-only-element file)))

(def layered-method make-alternatives ((component source-file/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'source-file/lisp-form-list/inspector :component-value value)
         (call-next-method)))

(def layered-method find-inspector-type-for-prototype ((prototype asdf:source-file))
  'source-file/inspector)

;;;;;;
;;; source-file/lisp-form-list/inspector

(def (component e) source-file/lisp-form-list/inspector (inspector/basic content/widget)
  ())

(def (macro e) source-file/lisp-form-list/inspector ((&rest args &key &allow-other-keys) &body file)
  `(make-instance 'source-file/lisp-form-list/inspector ,@args :component-value ,(the-only-element file)))

(def refresh-component source-file/lisp-form-list/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-file-into-string (asdf:component-pathname component-value))))))
