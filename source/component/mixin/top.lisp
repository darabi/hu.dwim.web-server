;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; top/component

(def (component e) top/component (content/mixin)
  ()
  (:documentation "A COMPONENT that is related to the FOCUS command."))

(def (function e) find-top-component (component &key (otherwise :error otherwise?))
  (or (find-ancestor-component-of-type 'top/component component :otherwise #f)
      (handle-otherwise (error "Unable to find top component starting from component ~A" component))))

(def (function e) top-component? (component)
  (check-type component component*)
  (eq component (find-top-component component :otherwise #f)))

(def (function e) find-top-component-content (component &key (otherwise :error otherwise?))
  (bind ((top-component (find-top-component component :otherwise nil)))
    (if top-component
        (content-of top-component)
        (handle-otherwise (error "Unable to find top component content starting from component ~A" component)))))

(def (function e) top-component-content? (component)
  (check-type component component*)
  (eq component (find-top-component-content component :otherwise #f)))
