;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; name-value-list/widget

(def (component e) name-value-list/widget (widget/basic
                                           contents/abstract)
  ())

(def (macro e) name-value-list/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'name-value-list/widget ,@args :contents (list ,@contents)))

(def render-xhtml name-value-list/widget
  <table (:class "name-value-list widget")
    ,(render-contents-for -self-)>)

;;;;;;
;;; name-value-group/widget

(def (component e) name-value-group/widget (widget/basic
                                            collapsible/abstract
                                            contents/abstract
                                            title/mixin
                                            frame-unique-id/mixin)
  ())

(def (macro e) name-value-group/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'name-value-group/widget ,@args :contents (list ,@contents)))

(def render-xhtml name-value-group/widget
  <tbody (:id ,(id-of -self-) :class "name-value-group widget")
    <tr <td ,(render-collapse-or-expand-command-for -self-)>
        <td (:colspan 2)
            ,(render-title-for -self-)>>
    ,(when (expanded-component? -self-)
       (foreach (lambda (content)
                  (bind ((id (generate-response-unique-string)))
                    <tr (:id ,id
                         :class "name-value-pair widget"
                         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
                         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
                      <td>
                      ,(render-component content)>))
                (contents-of -self-)))>)

;;;;;;
;;; name-value-pair/widget

(def (component e) name-value-pair/widget (widget/basic)
  ((name nil :type component)
   (value nil :type component)))

(def (macro e) name-value-pair/widget ((&rest args &key &allow-other-keys) &body name-and-value)
  (assert (length= 2 name-and-value))
  `(make-instance 'name-value-pair/widget ,@args
                  :name ,(first name-and-value)
                  :value ,(second name-and-value)))

(def render-xhtml name-value-pair/widget
  (bind (((:read-only-slots name value) -self-))
    <td (:class "name") ,(render-component name)>
    <td (:class "value") ,(render-component value)>))
