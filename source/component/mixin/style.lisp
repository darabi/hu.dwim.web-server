;;; (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; style-class/mixin

(def (component ea) style-class/mixin ()
  ((style-class nil))
  (:documentation "Generic STYLE classification support, rendered as the class attribute in XHTML."))

(def constructor style-class/mixin
  (bind (((:slots style-class) -self-))
    (unless style-class
      (setf style-class (component-style-class -self-)))))

;;;;;;
;;; custom-style/mixin

(def (component e) custom-style/mixin ()
  ((custom-style nil))
  (:documentation "Custom STYLE support on a per COMPONENT basis, rendered as the style attribute in XHTML."))

;;;;;;
;;; style/mixin

(def (component e) style/mixin (style-class/mixin custom-style/mixin)
  ())

(def with-macro* with-render-style/mixin (self &key (element-name "div"))
  (bind (((:read-only-slots style-class custom-style) self))
    <,element-name (:class ,style-class :style ,custom-style)
      ,(-body-)>))

;;;;;;
;;; style/component

(def (component e) style/component (style/mixin remote-setup/mixin)
  ()
  (:documentation "A COMPONENT with STYLE and remote setup."))

(def (with-macro* e) with-render-style/component (self &key (element-name "div"))
  (bind (((:read-only-slots id style-class custom-style) self))
    <,element-name (:id ,id :class ,style-class :style ,custom-style)
      ,(-body-)>))
