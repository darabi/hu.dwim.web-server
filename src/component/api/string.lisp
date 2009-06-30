;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; String as component

(def render-xhtml string
  `xml,-self-)

(def render-text string
  (write -self- :stream *text-stream*))

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
;;; Component value

(def method component-value-of ((self string))
  nil)

(def method (setf component-value-of) (new-value (self string))
  (values)
  #+nil
  (operation-not-supported))

(def method reuse-component-value ((self string) class prototype value)
  (values))

;;;;;;
;;; Refresh component

(def layered-method refresh-component ((self string))
  (values))

(def method to-be-refreshed-component? ((self string))
  #f)

(def method mark-to-be-refreshed-component ((self string))
  (values))

(def method mark-refreshed-component ((self string))
  (operation-not-supported))

;;;;;;
;;; Show/hide component

(def method visible-component? ((self string))
  #t)

(def method hide-component ((self string))
  (operation-not-supported))

(def method show-component ((self string))
  (values))

;;;;;;
;;; Clone component

(def method clone-component ((self string))
  self)

;;;;;;
;;; Print component

(def method print-component ((self string) &optional (*standard-output* *standard-output*))
  (prin1 self))
