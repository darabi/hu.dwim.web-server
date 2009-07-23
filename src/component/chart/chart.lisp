;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/chart

(def (component e) component/chart ()
  ()
  (:documentation "TODO"))

;;;;;;
;;; chart/abstract

(def (component e) chart/abstract ()
  ((configuration-provider
    :type (or symbol function))
   (data-provider
    nil
    :type (or symbol function))
   (width
    600
    :type number)
   (height
    400
    :type number)))

(def function render-chart (component kind)
  ;; TODO: move this to frame or something higher?
  (bind ((path (concatenate 'string "static/amCharts/" kind "/")))
    <script (:type "text/javascript" :src ,(concatenate 'string path "swfobject.js")) "">
    (bind ((id (generate-frame-unique-string))
           (data-provider (data-provider-of component)))
      ;; TODO: generate variable name
      <div (:id ,id) ,#"chart.missing-flash-plugin">
      `js(let ((chart (new SWFObject ,(concatenate 'string  path kind ".swf") ,kind ,(width-of component) ,(height-of component) "8")))
           (chart.addParam "wmode" "transparent")
           (chart.addVariable "path" ,path)
           (chart.addVariable "settings_file"
                              (encodeURIComponent ,(action/href (:delayed-content #t)
                                                                (funcall (configuration-provider-of component)))))
           ;; TODO this should work, fix qq
           ;;,(when data-provider
           ;;  `js(chart.addVariable "data_file"
           ;;                        (encodeURIComponent ,(action/href (:delayed-content #t)
           ;;                                                          (funcall (data-provider-of component))))))
           (unless ,(null data-provider)
             (chart.addVariable "data_file"
                                (encodeURIComponent ,(action/href (:delayed-content #t)
                                                                  (funcall (data-provider-of component))))))
           (chart.write ,id)))))

;; TODO add two variants instead of this: one that builds up the xml at creation time and serves the constant
;; and another one that delays and runs the xml building each time the request is served. 
(def macro make-xml-provider (&body forms)
  `(lambda ()
     (emit-http-response (("Content-Type" +xml-mime-type+))
       (emit-xml-prologue)
       ,@forms)))

(def function make-chart-from-files (type &key settings-file data-file)
  (make-instance type
                 :configuration-provider (lambda ()
                                           (make-file-serving-response settings-file))
                 :data-provider (lambda ()
                                  (make-file-serving-response data-file))))
