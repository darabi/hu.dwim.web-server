;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Dictionary basic

(def (component e) dictionary/basic ()
  ((names :type list :documentation "The list of symbols in the dictionary."))
  (:documentation "A DICTIONARY is a list of symbols with the related definitions."))

(def refresh-component dictionary/basic
  (not-yet-implemented))

(def render-component dictionary/basic
  (not-yet-implemented))
