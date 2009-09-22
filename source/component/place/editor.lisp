;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def layered-method make-place-editor (type &rest args &key &allow-other-keys)
  (apply #'make-place-inspector type :edited #t :editable #f args))
