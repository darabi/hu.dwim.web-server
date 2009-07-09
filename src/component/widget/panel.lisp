;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Panel widget

(def (component e) panel/widget (component-messages/widget
                                 content/abstract
                                 collapsible/abstract
                                 title-bar/mixin
                                 collapsible/mixin
                                 context-menu/mixin
                                 command-bar/mixin
                                 frame-unique-id/mixin)
  ()
  (:documentation "A COMPONENT with a TITLE-BAR, CONTEXT-MENU, COMPONENT-MESSAGEs, COMMANDs and another COMPONENT inside."))

(def (macro e) panel/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'panel/widget ,@args :content ,(the-only-element content)))

(def render-xhtml panel/widget
  (bind (((:read-only-slots content) -self-))
    (if (typep content 'reference/widget)
        (with-render-style/abstract (-self- :element-name "span")
          (render-context-menu-for -self-)
          (render-component-messages-for -self-)
          (render-content-for -self-))
        (if (expanded-component? -self-)
            (with-render-style/abstract (-self-)
              (render-context-menu-for -self-)
              (render-title-bar-for -self-)
              (render-component-messages-for -self-)
              <div ,(render-content-for -self-)>
              (render-command-bar-for -self-)
              (render-remote-setup -self-))
            (render-title-bar-for -self-)))))
