;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Pie chart

(def component pie-chart (chart)
  ())

(def render pie-chart ()
  (render-chart -self- "ampie"))

(def (function e) make-pie-chart (&key title names values width height colors
                                       (3d #f) (animation #f) (font-size 12))
  (make-instance 'pie-chart
                 :width width
                 :height height
                 :data-provider (make-xml-provider
                                  <pie ,(iter (for name :in names)
                                              (for value :in values)
                                              <slice (:title ,name) ,value>)>)
                 :configuration-provider (make-xml-provider
                                           <settings
                                            <font "Tahoma">
                                            <pie
                                             ,(when 3d
                                                <inner_radius 40>
                                                <height 20>
                                                <angle 30>)
                                             <gradient "radial">
                                             <gradient_ratio "50,0,0,-50">
                                             ,(when colors
                                                <colors ,(iter (for color :in colors)
                                                               (unless (first-iteration-p)
                                                                 `xml,",")
                                                               `xml,color)>)>
                                            ,(if animation
                                                 <animation
                                                  <start_time 2>
                                                  <start_effect "strong">
                                                  <pull_out_time 1.5>
                                                  <pull_out_effect "strong">
                                                  <pull_out_only_one "true">>)
                                            <data_labels
                                             <show "{title}: {percents}%">
                                             <line_color "#000000">
                                             <line_alpha 15>
                                             <hide_labels_percent 3>>
                                            <background
                                             <alpha "0">>
                                            <balloon
                                             <show "{title}: {value} ({percents}%)">>
                                            <legend
                                             <enabled "false">>
                                            ,(when title
                                               <labels
                                                   <label
                                                    <x 0>
                                                    <y 40>
                                                    <align "center">
                                                    <text_size ,font-size>
                                                    <text ,title>>>) >)))
