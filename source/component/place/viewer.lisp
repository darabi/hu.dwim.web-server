;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def layered-method make-place-viewer (type &rest args &key &allow-other-keys)
  (apply #'make-place-inspector type :edited #f :editable #f args))
