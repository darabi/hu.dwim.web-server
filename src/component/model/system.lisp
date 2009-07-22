;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; system/inspector

(def (component e) system/inspector (t/inspector)
  ())

(def layered-method find-inspector-type-for-prototype ((prototype asdf:system))
  'system/inspector)