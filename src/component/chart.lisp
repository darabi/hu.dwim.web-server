;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Chart

(def component chart ()
  ((configuration-provider)
   (data-provider nil)))

(def function render-chart (component kind &key (width 800) (height 400))
  ;; TODO: move this to frame or something higher?
  <script (:type "text/javascript" :src ,(concatenate 'string "/static/charting/" kind "/swfobject.js")) "">
  (bind ((id (generate-frame-unique-string))
         (data-provider (data-provider-of component)))
    ;; TODO: generate variable name
    <div (:id ,id) ,#"chart.missing-flash-plugin">
    `js(let ((variable (new SWFObject ,(concatenate 'string  "/static/charting/" kind "/" kind ".swf") ,kind ,width ,height "8" "#FFFFFF")))
         (.addVariable variable "settings_file"
                       (encodeURIComponent ,(make-action-href (:delayed-content #t)
                                              (funcall (configuration-provider-of component)))))
         (unless ,(null data-provider)
           (.addVariable variable "data_file"
                         (encodeURIComponent ,(make-action-href (:delayed-content #t)
                                                (funcall data-provider)))))
         (.write variable ,id))))

(def macro make-xml-provider (&body forms)
  `(lambda ()
     (emit-http-response (("Content-Type" +xml-mime-type+))
       (emit-xml-prologue +encoding+)
       ,@forms)))

(defresources en
  (chart.missing-flash-plugin "Flash Player is not available"))

(defresources hu
  (chart.missing-flash-plugin "Flash Player nem elérhető"))
