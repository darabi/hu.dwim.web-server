;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Panel widget

(def (component e) panel/widget (component-messages/widget content/abstract title-bar/mixin collapsible/mixin commands/mixin)
  ()
  (:documentation "A COMPONENT with a TITLE-BAR, CONTEXT-MENU, COMPONENT-MESSAGEs, COMMANDs and another COMPONENT inside."))

(def (layered-function e) render-panel (component)
  (:method ((self panel/widget))
    (bind (((:read-only-slots title-bar command-bar content id) self)
           (class-name (string-downcase (class-name (class-of self)))))
      (if (typep content '(or primitive-component reference-component))
          <span (:id ,id :class ,class-name)
                ,(render-component-messages-for self)
                ,(render-content-for self)>
          (progn
            <div (:id ,id :class ,class-name)
                 ,(render-component title-bar)
                 ,(render-component-messages-for self)
                 ,(render-content-for self)
                 ,(render-command-bar-for self)>
            (render-remote-setup self))))))

(def render-xhtml panel/widget
  (render-panel -self-))
