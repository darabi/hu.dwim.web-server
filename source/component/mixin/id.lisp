;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; id/mixin

(def (component e) id/mixin ()
  ((id :type string :documentation "A life time permanent string identifier."))
  (:documentation "A COMPONENT with a life time permanent string identifier."))

(debug-only
  (def method (setf id-of) :before (new-value (self id/mixin))
    (assert (not (id-of self)))))

;;;;;;
;;; frame-unique-id/mixin

(def (component e) frame-unique-id/mixin (refreshable/mixin id/mixin)
  ((id nil))
  (:documentation "A COMPONENT with a permanent frame unique string identifier that is set at the first REFRESH-COMPONENT and never changed afterwards."))

(def function generate-unique-component-id (&optional prefix)
  (if *frame*
      (generate-unique-string/frame prefix *frame*)
      (generate-unique-string/response prefix *response*)))

(def function ensure-frame-unique-id (component)
  (or (id-of component)
      (setf (id-of component) (generate-unique-component-id))))

(def layered-method refresh-component :before ((self frame-unique-id/mixin))
  (ensure-frame-unique-id self))

