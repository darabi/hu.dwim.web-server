;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree abstract

(def special-variable *tree*)

(def special-variable *tree-level*)

(def (component ea) tree/abstract ()
  ())

(def component-environment tree/basic
  (bind ((*tree* -self-)
         (*tree-level* -1))
    (call-next-method)))

(def (macro e) tree ((&rest args &key &allow-other-keys) &body rows)
  `(make-instance 'tree/full ,@args :rows (list ,@rows)))

;;;;;;
;;; Tree basic

(def (component ea) tree/basic (tree/abstract style/abstract)
  ((columns nil :type components)
   ;; TODO expander-column-index should be marked by a special column type, or something similar. this way it's very fragile...
   (expander-column-index 0 :type integer)
   (root-nodes nil :type components)
   (expand-nodes-by-default #f :type boolean)))

(def render-xhtml tree/basic
  (bind (((:read-only-slots root-nodes id) -self-))
    <table (:id ,id :class "tree")
      <thead <tr ,(render-tree-columns -self-) >>
      <tbody ,(foreach #'render-component root-nodes) >>))

(def render-csv tree/basic
  (write-csv-line (columns-of -self-))
  (write-csv-line-separator)
  (foreach #'render-component (root-nodes-of -self-)))

(def render-ods tree/basic
  <table:table
    <table:table-row ,(foreach #'render-component (columns-of -self-))>
    ,(foreach #'render-component (root-nodes-of -self-))>)

(def (layered-function e) render-tree-columns (tree/basic)
  (:method ((self tree/basic))
    (foreach #'render-component (columns-of self))))
