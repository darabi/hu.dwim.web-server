;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; commands/mixin

(def (component e) commands/mixin (menu-bar/mixin context-menu/mixin command-bar/mixin)
  ()
  (:documentation "A COMPONENT with various COMPONENTs providing behaviour through COMMANDs."))

(def (generic e) command-position (component)
  (:method ((self number))
    most-positive-fixnum)

  (:method ((self string))
    most-positive-fixnum)

  (:method ((self component))
    most-positive-fixnum)

  (:method ((self icon/widget))
    ;; TODO: can't we make it faster/better (what about a generic method or something?)
    (or (position (name-of self)
                  ;; TODO: this name thingie is quite fragile
                  '(answer back focus-out open-in-new-frame focus-in collapse-component refresh-component begin-editing save-editing cancel-editing store-editing revert-editing
                    new-instance delete-instance))
        most-positive-fixnum))

  (:method ((self command/widget))
    (command-position (content-of self)))

  (:method ((self menu-item/widget))
    (if (menu-items-of self)
        most-positive-fixnum
        (command-position (content-of self)))))
