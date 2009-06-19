;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Header mixin

(def (component e) header/mixin (title/mixin context-menu/mixin expandible/mixin visibility/mixin)
  ())

(def (layered-function e) render-header (component)
  (:method ((self header/mixin))
    (awhen (title-of self)
      )))
