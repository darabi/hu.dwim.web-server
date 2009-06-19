;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Movable abstract

(def (component e) movable/abstract ()
  ())

(def layered-method make-move-commands ((component movable/abstract) (class standard-class) (prototype standard-object) value)
  (optional-list (make-open-in-new-frame-command component class prototype value)
                 (make-focus-command component class prototype value)))

(def layered-method make-context-menu-items ((component movable/abstract) (class standard-class) (prototype standard-object) value)
  (append (call-next-method)
          (list (make-menu-item (icon menu :label "Mozgatás")
                                (make-move-commands component class prototype value)))))
