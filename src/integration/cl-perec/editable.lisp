;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization

(def layered-method make-editing-commands ((component editable-component) (class prc::persistent-class) (prototype prc::persistent-object) (instance prc::persistent-object))
  (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
    (call-next-method)))
