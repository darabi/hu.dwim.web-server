;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def hu.dwim.meta-model:operation replace-with-alternative-operation (hu.dwim.meta-model::standard-operation)
  ())

(def method make-replace-with-alternative-command :around ((component alternator/basic) alternative)
  (when (or (subtypep (the-class-of alternative) 'reference-component)
            (hu.dwim.meta-model::authorize-operation 'replace-with-alternative-operation))
    (call-next-method)))
