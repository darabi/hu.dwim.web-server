;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Id mixin

(def (component e) id/mixin ()
  ((id :type string :documentation "A life time unique identifier."))
  (:documentation "A component with a life time unique identifier."))

;;;;;;
;;; Frame unique id mixin

(def (component e) frame-unique-id/mixin (refreshable/mixin id/mixin)
  ()
  (:documentation "A component with a permanent frame unique identifier that is set at the first refresh and never changed afterwards."))

(def function ensure-frame-unique-id (component)
  (or (id-of component)
      (setf (id-of component) (generate-frame-unique-string "c"))))

(def layered-method refresh-component :before ((self frame-unique-id/mixin))
  (ensure-frame-unique-id self))

(def layered-method make-refresh-command ((component frame-unique-id/mixin) class prototype value)
  (aprog1 (call-next-method)
    (setf (ajax-of it) (ajax-id component))))

(def (layered-function e) ajax-id (component)
  (:method (self)
    #t)

  (:method ((self id/mixin))
    (ensure-frame-unique-id self)))
