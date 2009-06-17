;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Mouse abstract

(def (component ea) mouse/abstract ()
  ())

(def (layered-function e) render-onclick-handler (component button)
  (:method ((self mouse/abstract) button)
    (values)))
