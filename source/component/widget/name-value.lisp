;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

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

(def function is-name-value-group-collapsible? (self)
  (bind ((parent-component (parent-component-of self)))
    (or (not (typep parent-component 'name-value-list/widget))
        (not (length= 1 (contents-of parent-component))))))

(def render-xhtml name-value-group/widget
  <tbody (:id ,(id-of -self-) :class "name-value-group widget")
    ,(when (is-name-value-group-collapsible? -self-)
       <tr (:class "title")
         <td ,(render-collapse-or-expand-command-for -self-)>
         <td (:colspan 2) ,(render-title-for -self-)>>)
    ,(when (expanded-component? -self-)
       (foreach (lambda (content)
                  (bind ((id (generate-unique-component-id)))
                    <tr (:id ,id
                         :class "name-value-pair widget"
                         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
                         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
                      <td>
                      ,(render-component content)>))
                (contents-of -self-)))>)

(def method visible-child-component-slots ((self name-value-group/widget))
  (remove-slots (unless (is-name-value-group-collapsible? self)
                  '(expand-command collapse-command))
                (call-next-method)))

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
  (render-name-for -self-)
  (render-value-for -self-))

(def function render-name-for (component)
  <td (:class "name") ,(render-component (name-of component))>)

(def function render-value-for (component)
  <td (:class "value") ,(render-component (value-of component))>)
