;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; cloneable/abstract

(def (component e) cloneable/abstract ()
  ())

(def method clone-component ((self cloneable/abstract))
  (make-instance (class-of self)))

(def layered-method make-move-commands ((component cloneable/abstract) class prototype value)
  (optional-list* (make-open-in-new-frame-command component class prototype value) (call-next-method)))

(def layered-method open-in-new-frame ((component cloneable/abstract) class prototype value)
  (bind ((clone (clone-component component))
         (*frame* (make-new-frame *application* *session*)))
    (setf (id-of *frame*) (insert-with-new-random-hash-table-key (frame-id->frame-of *session*) *frame* +frame-id-length+))
    (register-frame *application* *session* *frame*)
    (setf (root-component-of *frame*) (make-frame-root-component clone))
    (make-redirect-response-with-frame-id-decorated *frame*)))

