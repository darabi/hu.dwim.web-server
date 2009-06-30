;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Layout abstract

(def (component e) layout/abstract ()
  ()
  (:documentation "A LAYOUT/ABSTRACT does not have any visual appearance on its own. If all CHILD-COMPONENTs within a LAYOUT/ABSTRACT are EMPTY/LAYOUTs, then the whole LAYOUT/ABSTRACT is practically INVISIBLE. A LAYOUT/ABSTRACT does not provide behaviour on the client side to modify its state."))

;;;;;;
;;; Layout minimal

(def (component e) layout/minimal (layout/abstract component/minimal)
  ())
