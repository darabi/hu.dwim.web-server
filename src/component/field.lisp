;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Checkbox field

(def function string-to-lisp-boolean (value)
  (eswitch (value :test #'string=)
    ("true" #t)
    ("false" #f)))

(def (function e) render-checkbox-field (value &key checked-image unchecked-image (id (unique-js-name "_chkb"))
                                               checked-tooltip unchecked-tooltip name value-sink)
  (assert (or (and (null checked-image) (null unchecked-image))
              (and checked-image unchecked-image)))
  (assert (not (and name value-sink)))
  (assert (or name value-sink))
  (when (typep name 'client-state-sink)
    (setf name (id-of name)))
  (bind ((name (if name
                   (etypecase name
                     (client-state-sink (id-of name))
                     (string name))
                   (progn
                     (assert (functionp value-sink))
                     (id-of (client-state-sink (client-value)
                              (funcall value-sink (string-to-lisp-boolean client-value)))))))
         (custom checked-image)
         (hidden-id (concatenate 'string id "_hidden")))
    <input (:id ,hidden-id
            :name ,name
            :value ,(if value "true" "false")
            :type "hidden")>
    (if custom
        (progn
          ;; TODO :tabindex (tabindex field)
          ;; :class (css-class field)
          <a (:id ,id)
            <img>>
          `js(on-load
              (wui.field.setup-custom-checkbox ,id ,checked-image ,unchecked-image ,checked-tooltip ,unchecked-tooltip)))
        (progn
          ;; TODO :accesskey (accesskey field)
          ;; :title (or (tooltip field) (if value
          ;;                                (enabled-tooltip-of field)
          ;;                                (disabled-tooltip-of field)))
          ;; :tabindex (tabindex field)
          ;; :class (css-class field)
          ;; :style (css-style field)
          <input (:id ,id
                  :type "checkbox"
                  ,(when value
                    (load-time-value (make-xml-attribute "checked" "") t)))>
          `js(on-load
              (wui.field.setup-simple-checkbox ,id ,checked-tooltip ,unchecked-tooltip))))))

;;;;;;
;;; String field

(def function render-string-field (type value client-state-sink)
  (bind ((id (generate-frame-unique-string "_w")))
    (render-dojo-widget (id)
      ;; TODO dojoRows 3
      <input (:type     ,type
              :id       ,id
              :name     ,(id-of client-state-sink)
              :value    ,value
              :dojoType #.+dijit/text-box+)>)))

;;;;;;
;;; Number field

(def function render-number-field (value client-state-sink)
  (bind ((id (generate-frame-unique-string "_w")))
    (render-dojo-widget (id)
      <input (:type     "text"
              :id       ,id
              :name     ,(id-of client-state-sink)
              :value    ,value
              :dojoType #.+dijit/number-text-box+)>)))

;;;;;;
;;; Select field

(def function render-select-field (value possible-values &key name (key #'identity) (test #'equal) (client-name-generator #'princ-to-string))
  (bind ((id (generate-frame-unique-string "_w")))
    (render-dojo-widget (id)
      <select (:id ,id
               :dojoType #.+dijit/filtering-select+
               :name ,name)
        ,(iter (for index :upfrom 0)
               (for possible-value :in-sequence possible-values)
               (for actual-value = (funcall key possible-value))
               (for client-name = (funcall client-name-generator actual-value))
               (for client-value = (integer-to-string index))
               <option (:value ,client-value
                        ,(when (funcall test value actual-value)
                           (make-xml-attribute "selected" "yes")))
                       ,client-name>)>)))
