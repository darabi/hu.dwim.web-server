;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/viewer

(def (component e) place/viewer (t/viewer place/presentation)
  ()
  (:documentation "An PLACE/VIEWER displays existing values of a TYPE at a PLACE."))
