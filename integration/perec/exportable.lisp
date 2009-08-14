;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (hu.dwim.meta-model:operation e) export-instance-operation (hu.dwim.meta-model::standard-operation)
  ())

;; TODO: shouldn't we put these customization on the dwim application layer?
(def layered-method make-export-command :around (format component (class standard-class) (prototype standard-object) (instance standard-object))
  (when (hu.dwim.meta-model::authorize-operation 'export-instance-operation :-entity- class :format format)
    (call-next-method)))
