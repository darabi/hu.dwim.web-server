;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; top/abstract

(def (component e) top/abstract (content/mixin)
  ()
  (:documentation "A COMPONENT that is related to the FOCUS command."))

(def (function e) find-top-component (component &key (otherwise :error otherwise?))
  (or (find-ancestor-component-with-type component 'top/abstract :otherwise #f)
      (handle-otherwise (error "Unable to find top component starting from component ~A" component))))

(def (function e) top-component? (component)
  (eq component (find-top-component component :otherwise #f)))

(def (function e) find-top-component-content (component &key (otherwise :error otherwise?))
  (awhen (or (find-top-component component :otherwise #f)
             (handle-otherwise (error "Unable to find top component content starting from component ~A" component)))
    (content-of it)))

(def (function e) top-component-content? (component)
  (eq component (find-top-component-content component :otherwise #f)))
