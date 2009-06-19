;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Paragraph basic

(def (component e) paragraph/basic (style/abstract content/abstract)
  ())

(def render-xhtml paragraph/basic
  (with-render-style/abstract (-self- :element-name "p")
    (call-next-method)))

;;;;;;
;;; Emphasize basic

(def (component e) emphasize/basic (style/abstract content/abstract)
  ())

(def render-xhtml emphasize/basic
  (with-render-style/abstract (-self- :element-name "span")
    (call-next-method)))
