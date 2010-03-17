;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Number as component

(def render-xhtml number
  `xml,-self-)

(def render-text number
  (write -self- :stream *text-stream*))

(def render-csv number
  (write-csv-value (princ-to-string -self-)))

(def render-ods number
  <text:p (office:value-type "string") ,-self->)

(def render-odt number
  <text:p (office:value-type "string") ,-self->)

;;;;;;
;;; Component dispatch class/prototype

(def method component-dispatch-class ((self number))
  (find-class 'number))

(def method component-dispatch-prototype ((self number))
  42)

;;;;;;
;;; Component documentation

(def method component-documentation ((self number))
  "A number is a valid component on its own.")

;;;;;;
;;; Component style

(def method component-style-class ((self number))
  nil)

;;;;;;
;;; Component value

(def method component-value-of ((self number))
  self)

(def method (setf component-value-of) (new-value (self number))
  (values))

(def method reuse-component-value ((component number) class prototype value)
  value)

;;;;;;
;;; Render component

(def method to-be-rendered-component? ((self number))
  #f)

(def method mark-to-be-rendered-component ((self number))
  (values))

(def method mark-rendered-component ((self number))
  (operation-not-supported "Cannot MARK-RENDERED-COMPONENT ~A"))

;;;;;;
;;; Refresh component

(def layered-method refresh-component ((self number))
  (values))

(def method to-be-refreshed-component? ((self number))
  #f)

(def method mark-to-be-refreshed-component ((self number))
  (values))

(def method mark-refreshed-component ((self number))
  (operation-not-supported "Cannot MARK-REFRESHED-COMPONENT ~A" self))

;;;;;;
;;; Show/hide component

(def method hideable-component? ((self number))
  #f)

(def method visible-component? ((self number))
  #t)

(def method hide-component ((self number))
  (operation-not-supported "Cannot HIDE-COMPONENT ~A" self))

(def method show-component ((self number))
  (values))

;;;;;;
;;; Enable/disable component

(def method disableable-component? ((self number))
  #f)

(def method enabled-component? ((self number))
  #t)

(def method disable-component ((self number))
  (operation-not-supported "Cannot DISABLE-COMPONENT ~A" self))

(def method enable-component ((self number))
  (values))

;;;;;;
;;; Expand/collapse component

(def method collapsible-component? ((self number))
  #f)

(def method expanded-component? ((self number))
  #t)

(def method collapse-component ((self number))
  (operation-not-supported "Cannot COLLAPSE-COMPONENT ~A" self))

(def method expand-component ((self number))
  (values))

;;;;;;
;;; Command

(def layered-method make-menu-bar-items ((component number) class prototype value)
  nil)

(def layered-method make-context-menu-items ((component number) class prototype value)
  nil)

(def layered-method make-command-bar-commands ((component number) class prototype value)
  nil)

;;;;;;
;;; Clone component

(def method clone-component ((self number))
  self)

;;;;;;
;;; Print component

(def method print-component ((self number) &optional (*standard-output* *standard-output*))
  (prin1 self))
