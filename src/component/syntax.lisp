;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;; TODO: kill ui prefix or what?

;;;;;;
;;; AST

(def ast ui)

(def class* ui-syntax-node (qq::syntax-node)
  ())

(def (class* e) ui-quasi-quote (quasi-quote ui-syntax-node)
  ())

(def (function e) make-ui-quasi-quote (transformation-pipeline body)
  (assert (not (typep body 'quasi-quote)))
  (make-instance 'ui-quasi-quote :transformation-pipeline transformation-pipeline :body body))

(def (class* e) ui-unquote (unquote ui-syntax-node)
  ())

(def (function e) make-ui-unquote (form &optional spliced?)
  (make-instance 'ui-unquote :form form :spliced spliced?))

;;;;;;
;;; Syntax

(define-syntax quasi-quoted-ui (&key (dispatched-quasi-quote-name "ui")
                                     (start-character #\[)
                                     (end-character #\])
                                     (unquote-character #\,)
                                     (splice-character #\@)
                                     (transformation-pipeline nil))
  (set-quasi-quote-syntax-in-readtable
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (cl-quasi-quote::run-transformation-pipeline (make-ui-quasi-quote transformation-pipeline (parse-quasi-quoted-ui body))))
   (lambda (form spliced)
     (make-ui-unquote form spliced))
   :nested-quasi-quote-wrapper
   (lambda (body dispatched?)
     (declare (ignore dispatched?))
     (parse-quasi-quoted-ui body))
   :dispatched-quasi-quote-name dispatched-quasi-quote-name
   :start-character start-character
   :end-character end-character
   :unquote-character unquote-character
   :splice-character splice-character))

(define-syntax quasi-quoted-ui-to-ui-emitting-form ()
  (set-quasi-quoted-ui-syntax-in-readtable :transformation-pipeline (list (make-instance 'quasi-quoted-syntax-node-to-syntax-node-emitting-form))))

(def method cl-quasi-quote::collect-slots-for-syntax-node-emitting-form ((node ui-syntax-node))
  (remove 'parent-component (class-slots (class-of node)) :key #'slot-definition-name))
