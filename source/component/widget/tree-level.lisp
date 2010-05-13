;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tree-level/widget

(def (component e) tree-level/widget (standard/widget collapsible/component)
  ((path nil :type component)
   (previous-sibling nil :type component)
   (next-sibling nil :type component)
   (descendants nil :type component)
   (node nil :type component)))

(def render-xhtml tree-level/widget
  (bind (((:read-only-slots path previous-sibling next-sibling descendants node) -self-))
    <div (:class "tree-level")
         <span (:class "header")
              ,(render-collapse-or-expand-command-for -self-)
              ,(when (expanded-component? -self-)
                     <span (:class "previous-sibling")
                           ,(when previous-sibling
                              (render-component previous-sibling)
                              (render-component " << "))>
                     <span (:class "path")
                           ,(render-component path)>
                     <span (:class "next-sibling")
                           ,(when next-sibling
                              (render-component " >> ")
                              (render-component next-sibling))>
                     <hr>)>
         ,(when (expanded-component? -self-)
            (render-component descendants)
            <hr>)
         ,(render-component node)>))
