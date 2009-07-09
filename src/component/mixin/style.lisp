;;; (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Style class mixin

(def (component e) style-class/mixin ()
  ((style-class nil))
  (:documentation "Generic STYLE classification support, rendered as the class attribute in XHTML."))

(def constructor style-class/mixin
  (bind (((:slots style-class) -self-))
    (unless style-class
      (setf style-class (string-downcase (substitute #\Space #\/ (symbol-name (class-name (class-of -self-)))))))))

;;;;;;
;;; Custom style mixin

(def (component e) custom-style/mixin ()
  ((custom-style nil))
  (:documentation "Custom STYLE support on a per COMPONENT basis, rendered as the style attribute in XHTML."))

;;;;;;
;;; Style mixin

(def (component e) style/mixin (style-class/mixin custom-style/mixin)
  ())

(def with-macro* with-render-style/mixin (self &key (element-name "div"))
  (bind (((:read-only-slots style-class custom-style) self))
    <,element-name (:class ,style-class :style ,custom-style)
      ,(-body-)>))

;;;;;;
;;; Style abstract

(def (component e) style/abstract (style/mixin remote-setup/mixin)
  ()
  (:documentation "A COMPONENT with STYLE and remote setup."))

(def with-macro* with-render-style/abstract (self &key (element-name "div"))
  (bind (((:read-only-slots id style-class custom-style) self))
    <,element-name (:id ,id :class ,style-class :style ,custom-style)
      ,(-body-)>))
