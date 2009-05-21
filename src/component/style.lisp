;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Style mixin

(def component style-mixin ()
  ((id nil)
   (css-class nil)
   (style nil))
  (:documentation "A component with a style class and individual style."))

;;;;;;
;;; Style component

(def component style-component (style-mixin content-mixin)
  ()
  (:documentation "A component with styles and a content inside."))

(def render-xhtml style-component
  (bind (((:read-only-slots id css-class style) -self-))
    <div (:id ,id :class ,css-class :style ,style)
         ,(call-next-method)>))

(def (macro e) style ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'style-component ,@args :content ,(the-only-element content)))
