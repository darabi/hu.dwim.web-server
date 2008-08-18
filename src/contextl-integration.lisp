;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;; this needs to be in a separate file to be able to load it early. this is like a
;; package.lisp, but it's for the wui asdf system (as opposed to wui-core).

(eval-always
  (use-package :contextl))

(def (function i) current-layer ()
  (contextl::layer-context-prototype (current-layer-context)))
