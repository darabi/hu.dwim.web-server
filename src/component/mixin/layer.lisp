;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Layer context capturing mixin

(def (component ea) layer-context-capturing/mixin ()
  ((layer-context (current-layer-context))))

(def component-environment layer-context-capturing/mixin
  (call-next-method)
  ;; TODO: revive layer capturing
  ;; FIXME: it overrides the rendering backend layer now
  #+nil(funcall-with-layer-context (layer-context-of -self-) #'call-next-method))

(def (function ie) current-layer ()
  (contextl::layer-context-prototype (current-layer-context)))
