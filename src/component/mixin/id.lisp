;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Id mixin

(def (component e) id/mixin ()
  ((id :type string :documentation "A life time unique string identifier."))
  (:documentation "A COMPONENT with a life time unique string identifier."))

;;;;;;
;;; Frame unique id mixin

(def (component e) frame-unique-id/mixin (refreshable/mixin id/mixin)
  ((id nil))
  (:documentation "A COMPONENT with a permanent frame unique string identifier that is set at the first REFRESH-COMPONENT and never changed afterwards."))

(def function ensure-frame-unique-id (component)
  (or (id-of component)
      (setf (id-of component) (generate-frame-unique-string "c"))))

(def layered-method refresh-component :before ((self frame-unique-id/mixin))
  (ensure-frame-unique-id self))

(def layered-method make-refresh-component-command ((component frame-unique-id/mixin) class prototype value)
  (aprog1 (call-next-method)
    (setf (ajax-of it) (ajax-of component))))

(def (generic e) ajax-of (component)
  (:method ((self component))
    #t)

  (:method ((self id/mixin))
    (ensure-frame-unique-id self)))

(def (generic e) (setf ajax-of) (new-value component))
