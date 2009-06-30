;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table abstract

(def special-variable *table*)

(def (component e) table/abstract ()
  ())

(def component-environment table/abstract
  (bind ((*table* -self-))
    (call-next-method)))
