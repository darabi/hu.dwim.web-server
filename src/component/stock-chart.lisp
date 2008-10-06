;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Stock chart

(def component stock-chart (chart)
  ())

(def render stock-chart ()
  (render-chart -self- "amstock"))

(def (function e) make-stock-chart (&key title file-name)
  (make-instance 'stock-chart
                 :configuration-provider (make-xml-provider
                                           <settings
                                            <data_sets
                                             <data_set
                                              <title "Belépésk napi bontásban">
                                              <short "Belépések">
                                              <file_name ,(make-action-href (:delayed-content #t) (make-file-serving-response file-name))>
                                              <csv
                                               <separator "|">
                                               <date_format "YYYY-MM-DD">
                                               <columns
                                                <column "date">
                                                <column "volume1">
                                                <column "volume2">>>>>
                                            <charts
                                             <chart
                                              <title ,title>
                                              <grid
                                               <x
                                                <alpha 10>
                                                <dashed "true">>
                                               <y_left
                                                <alpha 10>
                                                <dashed "true">
                                                <approx_count 5>>>
                                              <values
                                               <x <enabled "true">>>
                                              <graphs
                                               <graph
                                                <type "line">
                                                <data_sources
                                                 <close "volume1">>
                                                <cursor_color "002b6d">
                                                <fill_alpha 100>>>>>>)))
