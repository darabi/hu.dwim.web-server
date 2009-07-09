;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table widget

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
                                 page-navigation-bar/mixin
                                 frame-unique-id/mixin)
  ()
  (:documentation "A TABLE/WIDGET has several ROW/WIDGETs inside. It supports expanding, resizing, scrolling, page navigation, selection, highlighting and commands."))

(def (macro e) table/widget ((&rest args &key &allow-other-keys) &body rows)
  `(make-instance 'table/widget ,@args :rows (list ,@rows)))

(def refresh-component table/widget
  (bind (((:slots rows page-navigation-bar) -self-))
    (setf page-navigation-bar (page-navigation-bar/widget :total-count (length rows)))
    #+nil ;; TODO:
    (when (< (page-size-of page-navigation-bar) (total-count-of page-navigation-bar))
      (setf (total-count-of page-navigation-bar) (length rows)))))

(def render-xhtml table/widget
  (bind (((:read-only-slots rows page-navigation-bar id) -self-)
         (position (position-of page-navigation-bar))
         (visible-rows (subseq rows
                               position
                               (min (length rows)
                                    (+ position
                                       (page-size-of page-navigation-bar))))))
    <div (:id ,id :class "table widget")
         ,(render-context-menu-for -self-)
         <table
           <thead <tr ,(render-columns-for -self-)>>
           <tbody ,(iter (for index :from 0)
                         (for *row-index* = (+ position index))
                         (for row :in-sequence visible-rows)
                         (render-component row))>>
         ,(render-page-navigation-bar-for -self-)>))
