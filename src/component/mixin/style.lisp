;;; (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Style class mixin

(def (component ea) style-class/mixin ()
  ((style-class nil))
  (:documentation "Generic style classification support, rendered as the class attribute in XHTML."))

;;;;;;
;;; Custom style mixin

(def (component ea) custom-style/mixin ()
  ((custom-style nil))
  (:documentation "Custom per component style support, rendered as the style attribute in XHTML."))

;;;;;;
;;; Style mixin

(def (component ea) style/mixin (style-class/mixin custom-style/mixin)
  ())

;;;;;;
;;; Style abstract

(def (component ea) style/abstract (style/mixin remote-setup/mixin)
  ()
  (:documentation "A component with style and remote setup."))

(def with-macro* with-render-style/abstract (self &key (element-name "div"))
  (bind (((:read-only-slots id style-class custom-style) self))
    <,element-name (:id ,id :class ,style-class :style ,custom-style)
      ,(-body-)>))
