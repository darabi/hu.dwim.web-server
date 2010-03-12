;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; table/widget

(def (component e) table/widget (widget/style
                                 table/abstract
                                 rows/mixin
                                 columns/mixin
                                 selection/mixin
                                 command-bar/mixin
                                 context-menu/mixin
                                 resizable/mixin
                                 scrollable/mixin
                                 collapsible/mixin
                                 page-navigation-bar/mixin)
  ()
  (:documentation "A TABLE/WIDGET has several ROW/WIDGETs inside. It supports expanding, resizing, scrolling, page navigation, selection, highlighting and commands."))

(def (macro e) table/widget ((&rest args &key &allow-other-keys) &body rows)
  `(make-instance 'table/widget ,@args :rows (list ,@rows)))

(def render-xhtml table/widget
  (bind (((:read-only-slots rows page-navigation-bar) -self-)
         (position (position-of page-navigation-bar))
         (visible-rows (subseq rows
                               position
                               (min (length rows)
                                    (+ position
                                       (page-size-of page-navigation-bar))))))
    (with-render-style/abstract (-self-)
      (render-context-menu-for -self-)
      <table <thead <tr ,(render-columns-for -self-)>>
        <tbody ,(iter (for index :from 0)
                      (for *row-index* = (+ position index))
                      (for row :in-sequence visible-rows)
                      (render-component row))>>
      (render-page-navigation-bar-for -self-))))

(def layered-method make-page-navigation-bar ((component table/widget) class prototype value)
  (make-instance 'page-navigation-bar/widget :total-count (length (rows-of component))))
