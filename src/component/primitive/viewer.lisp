;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive viewer

(def (component e) primitive/viewer (primitive/abstract viewer/abstract)
  ())

;;;;;;
;;; Symbol viewer

(def (component e) symbol/viewer (symbol/abstract primitive/viewer)
  ())

(def render-xhtml symbol/viewer
  `xml,(print-component-value -self-))