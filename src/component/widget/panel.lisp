;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Header

(def (component e) header/mixin ()
  ((header :type component))
  (:documentation "A component with a header component."))

;;;;;;
;;; Footer

(def (component e) footer/mixin ()
  ((footer :type component))
  (:documentation "A component with a footer component."))

;;;;;;
;;; Panel basic

(def (component e) panel/basic (content/abstract title-bar/mixin visibility/mixin collapsible/mixin commands/mixin component-messages/basic)
  ()
  (:documentation "A COMPONENT with a TITLE-BAR, CONTEXT-MENU, COMPONENT-MESSAGEs, COMMANDs and another COMPONENT inside."))

(def (layered-function e) render-panel (component)
  (:method ((self panel/basic))
    (bind (((:read-only-slots title-bar command-bar content id) self)
           (class-name (string-downcase (class-name (class-of self)))))
      (if (typep content '(or primitive-component reference-component))
          <span (:id ,id :class ,class-name)
                ,(render-component-messages self)
                ,(render-component content)>
          (progn
            <div (:id ,id :class ,class-name)
                 ,(render-component title-bar)
                 ,(render-component-messages self)
                 ,(render-component content)
                 ,(render-component command-bar)>
            (render-remote-setup self))))))

(def render-xhtml panel/basic
  (render-panel -self-))
