;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; layer-context/mixin

(def (component e) layer-context/mixin ()
  ((layer-context (current-layer-context))))

(def component-environment layer-context/mixin
  (bind ((new-layer-context (adjoin-layer (contextl::layer-context-prototype (layer-context-of -self-)) (current-layer-context))))
    (funcall-with-layer-context new-layer-context #'call-next-method)))

(def (function ie) current-layer ()
  (contextl::layer-context-prototype (current-layer-context)))
