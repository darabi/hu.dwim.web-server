;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; String as component

(def render-xhtml string
  `xml,-self-)

(def render-text string
  (write-string -self- *text-stream*))

(def render-csv string
  (write-csv-value -self-))

(def render-ods string
  <text:p ,-self->)

(def render-odt string
  <text:p ,-self->)

;;;;;;
;;; Component dispatch class/prototype

(def method component-dispatch-class ((self string))
  (find-class 'string))

(def method component-dispatch-prototype ((self string))
  "42")

;;;;;;
;;; Component documentation

(def method component-documentation ((self string))
  "A string is a valid component on its own.")

;;;;;;
;;; Component style

(def method component-style-class ((self string))
  nil)

;;;;;;
;;; Component value

(def method component-value-of ((self string))
  nil)

(def method (setf component-value-of) (new-value (self string))
  (values))

(def method reuse-component-value ((self string) class prototype value)
  (values))

;;;;;;
;;; Render component

(def method to-be-rendered-component? ((self string))
  #f)

(def method mark-to-be-rendered-component ((self string))
  (values))

(def method mark-rendered-component ((self string))
  (operation-not-supported "Cannot MARK-RENDERED-COMPONENT ~A"))

;;;;;;
;;; Refresh component

(def layered-method refresh-component ((self string))
  (values))

(def method to-be-refreshed-component? ((self string))
  #f)

(def method mark-to-be-refreshed-component ((self string))
  (values))

(def method mark-refreshed-component ((self string))
  (operation-not-supported "Cannot MARK-REFRESHED-COMPONENT ~A" self))

;;;;;;
;;; Show/hide component

(def method hideable-component? ((self string))
  #f)

(def method visible-component? ((self string))
  #t)

(def method hide-component ((self string))
  (operation-not-supported "Cannot HIDE-COMPONENT ~A" self))

(def method show-component ((self string))
  (values))

;;;;;;
;;; Enable/disable component

(def method disableable-component? ((self string))
  #f)

(def method enabled-component? ((self string))
  #t)

(def method disable-component ((self string))
  (operation-not-supported "Cannot DISABLE-COMPONENT ~A" self))

(def method enable-component ((self string))
  (values))

;;;;;;
;;; Expand/collapse component

(def method collapsible-component? ((self string))
  #f)

(def method expanded-component? ((self string))
  #t)

(def method collapse-component ((self string))
  (operation-not-supported "Cannot COLLAPSE-COMPONENT ~A" self))

(def method expand-component ((self string))
  (values))

;;;;;;
;;; Command

(def layered-method make-menu-bar-items ((component string) class prototype value)
  nil)

(def layered-method make-context-menu-items ((component string) class prototype value)
  nil)

(def layered-method make-command-bar-commands ((component string) class prototype value)
  nil)

;;;;;;
;;; Clone component

(def method clone-component ((self string))
  self)

;;;;;;
;;; Print component

(def method print-component ((self string) &optional (*standard-output* *standard-output*))
  (prin1 self))
