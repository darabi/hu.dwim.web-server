;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def layered-method make-place-maker (type &rest args &key &allow-other-keys)
  (apply #'make-instance 'place/maker :component-value type args))