;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; column/chart

(def (component e) column/chart (standard/chart)
  ())

(def (macro e) column/chart ((&rest args &key &allow-other-keys) &body name-value-pairs)
  `(make-column-chart ,@args
                      :names (list ,@(mapcar #'first name-value-pairs))
                      :values (list ,@(mapcar #'second name-value-pairs))))

(def render-xhtml column/chart
  (render-chart -self- "amcolumn"))

(def (function e) make-column-chart (&key width height title names values)
  (declare (ignore title))
  (make-instance 'column/chart
                 :width width
                 :height height
                 :data-provider (make-xml-provider
                                  <chart <series ,(iter (for index :from 0)
                                                        (for name :in names)
                                                        <value (:xid ,index) ,name>)>
                                         <graphs <graph (:gid 1)
                                                        ,(iter (for index :from 0)
                                                               (for value :in values)
                                                               <value (:xid ,index) ,value>)>>>)
                 :configuration-provider (make-xml-provider
                                           <settings>)))
