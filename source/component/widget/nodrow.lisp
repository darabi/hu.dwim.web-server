;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; nodrow/widget

(def (component e) nodrow/widget (standard/widget
                                  node/component
                                  cells/mixin
                                  collapsible/component
                                  child-nodes/mixin
                                  context-menu/mixin
                                  collapsible/mixin
                                  selectable/mixin)
  ())

(def (macro e) nodrow/widget ((&rest args &key &allow-other-keys) &body child-nodes)
  `(make-instance 'nodrow/widget ,@args :child-nodes (list ,@child-nodes)))

(def render-xhtml nodrow/widget
  (bind (((:read-only-slots child-nodes expanded-component id custom-style) -self-))
    <tr (:id ,id :style ,custom-style :class `str("content " ,(nodrow-style-class -self-)
                                                             ,(selectable-component-style-class -self-)
                                                             ,(unless child-nodes " leaf"))
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-nodrow-cells -self-)>
    (render-onclick-handler -self- :left)
    (render-context-menu-for -self-)
    (when expanded-component
      (foreach #'render-component child-nodes))))

(def render-csv nodrow/widget
  (write-csv-line (cells-of -self-))
  (write-csv-line-separator)
  (foreach #'render-component (child-nodes-of -self-)))

(def render-ods nodrow/widget
  (bind (((:read-only-slots child-nodes expanded-component) -self-))
    <table:table-row ,(render-nodrow-cells -self-)>
    (when (and child-nodes
               expanded-component)
      <table:table-row-group ,(foreach #'render-component (child-nodes-of -self-))>)))

(def layered-method render-onclick-handler ((self nodrow/widget) (button (eql :left)))
  (when-bind select-command (find-command self 'select-component)
    (render-command-onclick-handler select-command (id-of self))))

(def (layered-function e) nodrow-style-class (component)
  (:method ((self nodrow/widget))
    (string+ "level-" (integer-to-string *tree-level*) " " (style-class-of self))))

(def (function e) render-nodrow-expander (nodrow)
  (bind (((:slots child-nodes expanded-component) nodrow)
         (treeble *tree*))
    (if child-nodes
        (render-command/xhtml (make-action
                                (notf expanded-component)
                                ;; NOTE: we make dirty the whole treeble, because it is difficult to replace the rows corresponding to the nodrow
                                (mark-to-be-rendered-component treeble))
                              (make-instance 'icon/widget
                                             :name (if expanded-component
                                                       'collapse-component
                                                       'expand-component)
                                             :label nil)
                              :subject-dom-node (id-of treeble))
        <span (:class "non-expandable")>)))

(def (function e) render-nodrow-expander-cell (nodrow)
  (bind ((expander-cell (elt (cells-of nodrow) (expander-column-index-of *tree*))))
    <td (:class "expander cell widget")
        ,(render-nodrow-expander nodrow)
        ,(typecase expander-cell
           (string
             (render-component expander-cell))
           (content/mixin
             (ensure-refreshed expander-cell)
             (render-component (content-of expander-cell)))
           (t
             (render-component expander-cell)))>))

(def (layered-function e) render-nodrow-cells (component)
  (:method ((self nodrow/widget))
    (iter (with expander-column-index = (expander-column-index-of *tree*))
          (for index :from 0)
          (for cell :in (cells-of self))
          (for column :in (columns-of *tree*))
          (when (visible-component? column)
            (if (= index expander-column-index)
                (render-nodrow-expander-cell self)
                (render-table-row-cell *tree* self column cell))))))

;; TODO: rename and factor into mixin/component classes and with the one found in cell.lisp
(def layered-methods render-table-row-cell
  (:method :before ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell cell/widget))
    (ensure-refreshed cell))

  (:method ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell cell/widget))
    (render-component cell))

  (:method :in xhtml-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell component))
    <td (:class "cell widget") ,(render-component cell)>)

  (:method :in xhtml-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell string))
    <td (:class "cell widget") ,(render-component cell)>)

  (:method :in ods-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell component))
    <table:table-cell ,(render-component cell)>)

  (:method :in ods-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell string))
    <table:table-cell ,(render-component cell)>))

;;;;;;
;;; entire-nodrow/widget

(def (component e) entire-nodrow/widget (standard/widget
                                         node/component
                                         content/component
                                         collapsible/component
                                         context-menu/mixin
                                         collapsible/mixin
                                         selectable/mixin)
  ())

(def (macro e) entire-nodrow/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'entire-nodrow/widget ,@args :content ,(the-only-element content)))

(def layered-method render-onclick-handler ((self entire-nodrow/widget) (button (eql :left)))
  nil)

(def render-xhtml entire-nodrow/widget
  (bind (((:read-only-slots id) -self-))
    (with-render-style/component (-self- :element-name "tr")
      <td (:colspan ,(length (columns-of *tree*))
           :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
           :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
        ,(render-content-for -self-)>)))
