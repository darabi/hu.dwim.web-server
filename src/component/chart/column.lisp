;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Column chart

(def (component ea) column-chart (chart)
  ())

(def render-xhtml column-chart
  (render-chart -self- "amcolumn"))

(def (function e) make-column-chart (&key height width names values)
  (make-instance 'column-chart
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
