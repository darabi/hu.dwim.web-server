;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Paragraph basic

(def (component e) paragraph/basic (contents/abstract style/mixin)
  ((style-class "paragraph")))

(def macro paragraph/basic ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'paragraph/basic ,@args :contents (list ,@contents)))

(def render-xhtml paragraph/basic
  (with-render-style/mixin (-self- :element-name "p")
    (call-next-method)))

(def layered-function render-paragraph (component)
  (:method :in xhtml-layer ((self number))
    <p ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <p ,(render-component self)>)

  (:method ((self component))
    (render-component self)))

;;;;;;
;;; Paragraph

(def macro paragraph ((&rest args &key &allow-other-keys) &body contents)
  `(paragraph/basic ,args ,@contents))

;;;;;;
;;; Emphasize basic

(def (component e) emphasize/basic (contents/abstract style/mixin)
  ((style-class "emphasize")))

(def macro emphasize/basic ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'emphasize/basic ,@args :contents (list ,@contents)))

(def render-xhtml emphasize/basic
  (with-render-style/mixin (-self- :element-name "span")
    (call-next-method)))

;;;;;;
;;; Emphasize

(def macro emphasize ((&rest args &key &allow-other-keys) &body contents)
  `(emphasize/basic ,args ,@contents))
