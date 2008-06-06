;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;; TODO: a lot is missing

;;;;;;
;;; Tree

(def component tree-component ()
  ((columns :type components)
   (root :type component)))

(def render tree-component ()
  (with-slots (columns root) -self-
    <table
      <thead
        <tr ,@(mapcar #'render columns)>>
      <tbody ,(render root)>>))

(def component node-component ()
  ((children :type components)
   (cells :type components)))

(def render node-component ()
  (with-slots (children cells) -self-
    (append (list <tr ,(mapcar #'render cells)>)
            (mapcar #'render children))))

;;;;;;
;;; Hierarchy

(def component hierarchy-component ()
  ((root)))

(def (macro e) hierarchy-component (root)
  `(make-instance 'hierarchy-component :root ,root))

(def special-variable *hierarchy-level* 0)

(def render hierarchy-component ()
  (labels ((render-node (node)
             (bind ((*hierarchy-level* (1+ *hierarchy-level*))
                    (children (children-in-hierarchy node)))
               (if children
                   (if (oddp *hierarchy-level*)
                       <table (:class "node")
                         <thead <tr <td ,(render-hierarchy-node -self- node) >>>
                         ,@(mapcar (lambda (child)
                                     <tr <td ,(render-node child)>>)
                                   children)>
                       <table (:class "node")
                         <thead <tr <td (:colspan ,(length children))
                                        ,(render-hierarchy-node -self- node)>>>
                         <tr ,@(mapcar (lambda (child)
                                         <td ,(render-node child)>) children)>>)
                   <table (:class "leaf")
                     <tr <td ,(render-hierarchy-node -self- node)>>>))))
    <div (:class "hierarchy")
         ,(render-node (root-of -self-))>))

(def (generic e) children-in-hierarchy (node))

(def (generic e) render-hierarchy-node (component node))
