;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Package inspector

(def (component e) package/inspector ()
  ())

(def render-xhtml package/inspector
  ;; TODO: specialize render (e.g. documentation)
  (call-next-method))
