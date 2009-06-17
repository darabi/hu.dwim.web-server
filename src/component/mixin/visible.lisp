;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Visible mixin

;; TODO: visibility/mixin
(def (component ea) visible/mixin ()
  ((visible
    #t
    :type boolean
    :computed-in compute-as
    :documentation "TRUE means the component must be visible on the remote side, FALSE means the opposite."))
  (:documentation "A component that can be hidden or shown."))

(def render :around visible/mixin
  (when (visible? -self-)
    (call-next-method)))

;; TODO: render-toggle-visibility-command
(def (layered-function e) render-visible-handle (component)
  (:method ((self visible/mixin))
    (render-component (command ()
                        (if (visible? self) "-" "+")
                        (make-action (notf (visible? self)))))))

(def method hide-component ((self visible/mixin))
  (setf (visible? self) #f))

(def method show-component ((self visible/mixin))
  (setf (visible? self) #t))

(def method show-component-tree ((self visible/mixin))
  (map-descendant-components self
                             (lambda (descendant)
                               (bind ((slot (find-slot (class-of descendant) 'visible))
                                      (slot-value (standard-instance-access descendant (slot-definition-location slot))))
                                 (when (eq #t slot-value) ; avoid making visible components with computed visible flags
                                   (show-component descendant))))))

(def layered-method make-hide-command ((component visible/mixin) class prototype value)
  (command ()
    (icon hide)
    (make-action (hide-component component))))

(def layered-method make-show-command ((component visible/mixin) class prototype value)
  (command ()
    (icon hide)
    (make-action (show-component component))))

(def layered-method make-show-component-tree-command ((component visible/mixin) class prototype value)
  (command ()
    (icon hide)
    (make-action (show-component-tree component))))

(def layered-method make-context-menu-items ((component visible/mixin) class prototype value)
  (list* (menu-item ()
             (icon menu :label "Show/Hide")
           (make-hide-command component class prototype value)
           (make-show-component-tree-command component class prototype value))
         (call-next-method)))
