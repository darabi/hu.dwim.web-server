;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

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
  <text:p ,-self->)

(def render-odt number
  <text:p ,-self->)

;;;;;;
;;; Component dispatch class/prototype

(def method component-dispatch-class ((self number))
  (find-class 'number))

(def method component-dispatch-prototype ((self number))
  42)

;;;;;;
;;; Component value

(def method component-value-of ((self number))
  nil)

(def method (setf component-value-of) (new-value (self number))
  (values)
  #+nil
  (operation-not-supported))

(def method reuse-component-value ((self number) class prototype value)
  (values))

;;;;;;
;;; Refresh component

(def layered-method refresh-component ((self number))
  (values))

(def method to-be-refreshed-component? ((self number))
  #f)

(def method mark-to-be-refreshed-component ((self number))
  (values))

(def method mark-refreshed-component ((self number))
  (operation-not-supported))

;;;;;;
;;; Show/hide component

(def method hideable-component? ((self number))
  #f)

(def method visible-component? ((self number))
  #t)

(def method hide-component ((self number))
  (operation-not-supported))

(def method show-component ((self number))
  (values))

;;;;;;
;;; Enable/disable component

(def method enableable-component? ((self number))
  #f)

(def method enabled-component? ((self number))
  #t)

(def method disable-component ((self number))
  (operation-not-supported))

(def method enable-component ((self number))
  (values))

;;;;;;
;;; Expand/collapse component

(def method expandible-component? ((self number))
  #f)

(def method expanded-component? ((self number))
  #t)

(def method collapse-component ((self number))
  (operation-not-supported))

(def method expand-component ((self number))
  (values))

;;;;;;
;;; Clone component

(def method clone-component ((self number))
  self)

;;;;;;
;;; Print component

(def method print-component ((self number) &optional (*standard-output* *standard-output*))
  (prin1 self))
