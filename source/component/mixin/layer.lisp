;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; layer/mixin

(def (component e) layer/mixin ()
  ((layer (current-layer))))

(def component-environment layer/mixin
  (funcall-with-layer-context (adjoin-layer (layer-of -self-) (current-layer-context)) #'call-next-method))

(def (function ie) current-layer ()
  (contextl::layer-context-prototype (current-layer-context)))
