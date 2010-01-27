;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; splitter/widget

(def (component e) splitter/widget (widget/style list/layout)
  ())

(def (macro e) splitter/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'splitter/widget ,@args :contents (list ,@contents)))

(def render-xhtml splitter/widget
  (bind (((:read-only-slots id style-class custom-style orientation contents) -self-))
    (render-dojo-widget (id)
      <div (:id ,id
            :class ,style-class
            :style ,custom-style
            :dojoType #.+dijit/split-container+
            :orientation ,(string-downcase orientation)
            :sizerWidth 7)
        ,(foreach (lambda (content)
                    (bind ((pane-id (generate-unique-component-id)))
                      (render-dojo-widget (pane-id)
                        <div (:id ,pane-id
                              :dojoType #.+dijit/content-pane+
                              :sizeMin 10
                              :sizeShare 50)
                             <p <div "FOOO">
                             <div "BAR" >>
                          ,(render-component content)>)))
                  contents)>)))
