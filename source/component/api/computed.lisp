;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Computed universe

(def (computed-universe e) (computed-universe/session
                            :computed-state-factory-name compute-as
                            :universe-accessor-form (computed-universe-of *session*)))
