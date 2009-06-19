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
;;; Refresh component

(def layered-method refresh-component ((self number))
  (values))

(def method to-be-refreshed-component? ((self number))
  #f)

(def method mark-component-to-be-refreshed ((self number))
  (operation-not-supported))

(def method mark-component-refreshed ((self number))
  (operation-not-supported))

;;;;;;
;;; Show/hide component

(def method visible-component? ((self number))
  #t)

(def method hide-component ((self number))
  (operation-not-supported))

(def method show-component ((self number))
  (values))

;;;;;;
;;; Clone component

(def method clone-component ((self number))
  self)

;;;;;;
;;; Print component

(def method print-component ((self number) &optional (*standard-output* *standard-output*))
  (write-string (princ-to-string self)))
