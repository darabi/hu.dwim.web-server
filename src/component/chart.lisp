;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Chart

(def component chart ()
  ((configuration-provider)
   (data-provider nil)
   (width 800)
   (height 400)))

(def function render-chart (component kind)
  ;; TODO: move this to frame or something higher?
  
  (bind ((path (concatenate 'string "static/charting/" kind "/")))
    <script (:type "text/javascript" :src ,(concatenate 'string path "swfobject.js")) "">
    (bind ((id (generate-frame-unique-string))
           (data-provider (data-provider-of component)))
      ;; TODO: generate variable name
      <div (:id ,id) ,#"chart.missing-flash-plugin">
      `js(let ((variable (new SWFObject ,(concatenate 'string  path kind ".swf") ,kind ,(width-of component) ,(height-of component) "8" "#FFFFFF")))
           (.addVariable variable "path" ,path)
           (.addVariable variable "settings_file"
                         (encodeURIComponent ,(action/href (:delayed-content #t)
                                                (funcall (configuration-provider-of component)))))
           (unless ,(null data-provider)
             (.addVariable variable "data_file"
                           (encodeURIComponent ,(action/href (:delayed-content #t)
                                                  (funcall (data-provider-of component))))))
           (.write variable ,id)))))

(def macro make-xml-provider (&body forms)
  `(lambda ()
     (emit-http-response (("Content-Type" +xml-mime-type+))
       (emit-xml-prologue +encoding+)
       ,@forms)))

(def resources en
  (chart.missing-flash-plugin "Flash Player is not available"))

(def resources hu
  (chart.missing-flash-plugin "Flash Player nem elérhető"))


(def function make-chart-from-files (type &key settings-file-relative-path data-file-relative-path)
  (make-instance type
                 :configuration-provider (lambda () (make-file-serving-response
                                                (merge-pathnames settings-file-relative-path (asdf::component-pathname (asdf::find-system :wui))) ))
                 :data-provider (lambda () (make-file-serving-response
                                       (merge-pathnames data-file-relative-path (asdf::component-pathname (asdf::find-system :wui)))))))