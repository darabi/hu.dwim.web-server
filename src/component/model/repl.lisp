;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; lisp-form-list/repl/inspector

(def (component e) lisp-form-list/repl/inspector (list/inspector)
  ())

(def (macro e) lisp-form-list/repl/inspector ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'lisp-form-list/repl/inspector ,@args :component-value ',forms))

(def method make-list/element ((component lisp-form-list/repl/inspector) class prototype value)
  (lisp-form/invoker ()
    value))

(def method add-list-element ((component lisp-form-list/repl/inspector) class prototype value)
  (lisp-form/invoker ()
    (print "Hello World")))
