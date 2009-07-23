;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; nodrow/widget

(def (component e) nodrow/widget (widget/style
                                  node/abstract
                                  cells/mixin
                                  collapsible/abstract
                                  child-nodes/mixin
                                  context-menu/mixin
                                  collapsible/mixin
                                  selectable/mixin)
  ())

(def (macro e) nodrow/widget ((&rest args &key &allow-other-keys) &body child-nodes)
  `(make-instance 'nodrow/widget ,@args :child-nodes (list ,@child-nodes)))

(def render-xhtml nodrow/widget
  (bind (((:read-only-slots child-nodes expanded-component id custom-style) -self-)
         (onclick-handler? (render-onclick-handler -self- :left)))
    <tr (:id ,id :style ,custom-style :class ,(concatenate-string (nodrow-style-class -self-) (when onclick-handler? " selectable"))
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-nodrow-cells -self-) >
    (when expanded-component
      (foreach #'render-component child-nodes))))

(def render-csv nodrow/widget
  (write-csv-line (cells-of -self-))
  (write-csv-line-separator)
  (foreach #'render-component (child-nodes-of -self-)))

(def render-ods nodrow/widget
  <table:table-row ,(foreach (lambda (column cell)
                               (render-ods-tree-cell *tree* -self- column cell))
                             (columns-of *tree*)
                             (cells-of -self-))>
  (awhen (child-nodes-of -self-)
    <table:table-row-group ,(foreach #'render-component (child-nodes-of -self-))>))

(def layered-method render-onclick-handler ((self nodrow/widget) (button (eql :left)))
  nil)

(def (layered-function e) nodrow-style-class (component)
  (:method ((self nodrow/widget))
    (concatenate-string "level-" (integer-to-string *tree-level*) " " (style-class-of self))))

(def (function e) render-nodrow-expander (nodrow)
  (bind (((:slots child-nodes expanded-component) nodrow)
         (treeble *tree*))
    (if child-nodes
        (bind ((id (generate-response-unique-string)))
          <img (:id ,id :src ,(concatenate-string (path-prefix-of *application*)
                                                  (if expanded-component
                                                      "static/wui/icon/20x20/arrowhead-down.png"
                                                      "static/wui/icon/20x20/arrowhead-right.png")))>
          `js(on-load (dojo.connect (dojo.by-id ,id) "onclick" nil
                                    (lambda (event)
                                      (wui.io.action ,(action/href ()
                                                        (notf expanded-component)
                                                        ;; NOTE: we make dirty the whole treeble, because it is difficult to replace the rows corresponding to the nodrow
                                                        (mark-to-be-rendered-component treeble))
                                                     :event event
                                                     :ajax ,(when (ajax-enabled? *application*)
                                                              (id-of treeble)))))))
        <span (:class "non-expandable")>)))

(def (function e) render-nodrow-expander-cell (nodrow)
  (bind ((expander-cell (elt (cells-of nodrow) (expander-column-index-of *tree*))))
    <td (:class "expander")
        ,(render-nodrow-expander nodrow)
        ,(if (stringp expander-cell)
             (render-component expander-cell)
             (progn
               (ensure-refreshed expander-cell)
               (render-component (content-of expander-cell))))>))

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

;; TODO: rename and factor into mixin/abstract classes
(def layered-methods render-table-row-cell
  (:method :before ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell cell/widget))
    (ensure-refreshed cell))

  (:method ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell cell/widget))
    (render-component cell))

  (:method :in xhtml-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell component))
    <td ,(render-component cell)>)

  (:method :in xhtml-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell string))
    <td ,(render-component cell)>)

  (:method :in ods-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell component))
    <table:table-cell ,(render-component cell)>)

  (:method :in ods-layer ((table treeble/widget) (row nodrow/widget) (column column/widget) (cell string))
    <table:table-cell ,(render-component cell)>))

;;;;;;
;;; entire-nodrow/widget

(def (component e) entire-nodrow/widget (widget/style
                                         node/abstract
                                         content/abstract
                                         collapsible/abstract
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
    (with-render-style/abstract (-self- :element-name "tr")
      <td (:colspan ,(length (columns-of *tree*))
           :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
           :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
        ,(render-content-for -self-)>)))
