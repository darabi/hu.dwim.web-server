;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Treeble widget

(def (component e) treeble/widget (tree/abstract
                                   widget/style
                                   root-nodes/mixin
                                   columns/mixin
                                   collapsible/mixin
                                   selection/mixin)
  (;; TODO expander-column-index should be marked by a special column type, or something similar. this way it's very fragile...
   (expander-column-index 0 :type integer)
   (expand-nodes-by-default #f :type boolean)))

(def (macro e) treeble/widget ((&rest args &key &allow-other-keys) &body root-nodes)
  `(make-instance 'treeble/widget ,@args :root-nodes (list ,@root-nodes)))

(def render-xhtml treeble/widget
  (bind (((:read-only-slots root-nodes) -self-))
    (with-render-style/abstract (-self- :element-name "table")
      <thead <tr ,(render-treeble-columns -self-)>>
      <tbody ,(foreach #'render-component root-nodes)>)))

(def render-csv treeble/widget
  (write-csv-line (columns-of -self-))
  (write-csv-line-separator)
  (foreach #'render-component (root-nodes-of -self-)))

(def render-ods treeble/widget
  <table:table
    <table:table-row ,(foreach #'render-component (columns-of -self-))>
    ,(foreach #'render-component (root-nodes-of -self-))>)

(def (layered-function e) render-treeble-columns (treeble)
  (:method ((self treeble/widget))
    (foreach #'render-component (columns-of self))))
