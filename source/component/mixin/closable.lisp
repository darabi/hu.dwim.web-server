;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; closable/component

(def (component e) closable/component ()
  ()
  (:documentation "A COMPONENT that is permanently closable by explicit user interaction."))

(def layered-method make-move-commands ((component closable/component) class prototype value)
  (optional-list* (make-close-component-command component class prototype value) (call-next-layered-method)))

(def method close-component ((component closable/component) class prototype value)
  (execute-replace (make-component-place component) (make-instance 'empty/layout)))
