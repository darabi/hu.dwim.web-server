;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Computed universe

(def computed-universe (computed-universe/session-local
                        :computed-state-factory-name compute-as
                        :universe-accessor-form (computed-universe-of *session*)))

;; TODO: KLUDGE: wtf? once upon a time there was an export flag for computed-universe
(export 'compute-as)
