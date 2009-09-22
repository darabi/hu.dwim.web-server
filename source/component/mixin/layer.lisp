;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; layer-context-capturing/mixin

(def (component e) layer-context-capturing/mixin ()
  ((layer-context (current-layer-context))))

(def component-environment layer-context-capturing/mixin
  (call-next-method)
  ;; TODO: revive layer capturing
  ;; FIXME: currently this is commented out, because it overrides the rendering backend
  #+nil(funcall-with-layer-context (layer-context-of -self-) #'call-next-method))

(def (function ie) current-layer ()
  (contextl::layer-context-prototype (current-layer-context)))