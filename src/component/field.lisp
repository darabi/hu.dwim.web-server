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
                                               checked-tooltip unchecked-tooltip
                                               checked-class unchecked-class
                                               on-change name value-sink)
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
         (custom (or checked-image checked-class))
         (hidden-id (concatenate 'string id "_hidden"))
         (checked (when value "")))
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
              (wui.field.setup-custom-checkbox ,id ,checked-image ,unchecked-image ,checked-tooltip ,unchecked-tooltip ,checked-class ,unchecked-class)))
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
                  :onChange ,(force on-change)
                  :checked ,checked)>
          `js(on-load
              (wui.field.setup-simple-checkbox ,id ,checked-tooltip ,unchecked-tooltip))))))

;;;;;;
;;; String field

(def function render-string-field (type value client-state-sink &key on-change)
  (bind ((id (generate-frame-unique-string "_w")))
    (render-dojo-widget (id)
      ;; TODO dojoRows 3
      <input (:type     ,type
              :id       ,id
              :name     ,(id-of client-state-sink)
              :value    ,value
              :onChange ,(force on-change)
              :dojoType #.+dijit/text-box+)>)))

;;;;;;
;;; Number field

(def function render-number-field (value client-state-sink &key on-change)
  (bind ((id (generate-frame-unique-string "_w")))
    (render-dojo-widget (id)
      <input (:type     "text"
              :id       ,id
              :name     ,(id-of client-state-sink)
              :value    ,value
              :onChange ,(force on-change)
              :dojoType #.+dijit/number-text-box+)>)))

;;;;;;
;;; Combo box

(def function render-combo-box-field (value possible-values &key name (key #'identity) (test #'equal) (client-name-generator #'princ-to-string))
  (bind ((id (generate-frame-unique-string "_w")))
    <select (:id ,id
             :name ,name)
      ,(iter (for index :upfrom 0)
             (for possible-value :in-sequence possible-values)
             (for actual-value = (funcall key possible-value))
             (for client-name = (funcall client-name-generator actual-value))
             (for client-value = (integer-to-string index))
             (for selected = (when (funcall test value actual-value) "yes"))
             <option (:value ,client-value :selected ,selected)
                     ,client-name>)>))

;;;;;;
;;; Select field

(def function render-select-field (value possible-values &key name (key #'identity) (test #'equal) (client-name-generator #'princ-to-string) on-change)
  (bind ((id (generate-frame-unique-string "_w")))
    (render-dojo-widget (id)
      <select (:id ,id
               :dojoType #.+dijit/filtering-select+
               :name ,name
               :onChange ,(force on-change))
        ,(iter (for index :upfrom 0)
               (for possible-value :in-sequence possible-values)
               (for actual-value = (funcall key possible-value))
               (for client-name = (funcall client-name-generator actual-value))
               (for client-value = (integer-to-string index))
               (for selected = (when (funcall test value actual-value) "yes"))
               <option (:value ,client-value :selected ,selected)
                       ,client-name>)>)))

;;;;;;
;;; Popup menu select field

(def function render-popup-menu-select-field (value possible-values &key value-sink classes (test #'equal) (key #'identity))
  (bind ((div-id (generate-frame-unique-string))
         (menu-id (generate-frame-unique-string))
         (field-id (generate-frame-unique-string))
         (name (id-of (client-state-sink (client-value)
                        (funcall value-sink client-value))))
         (index (position value possible-values :key key :test test)))
    <input (:id ,field-id :type "hidden" :name ,name :value ,value)>
    <div (:id ,div-id :class ,(nth index classes))
      ,(unless classes
         value)
      ,(render-dojo-widget (menu-id)
        <div (:id ,menu-id
              :dojoType #.+dijit/menu+
              :leftClickToOpen "true"
              :style "display: none;"
              :targetNodeIds ,div-id)
          ,(iter (for possible-value :in possible-values)
                 (for class :in classes)
                 (for option-id = (generate-frame-unique-string))
                 (render-dojo-widget (option-id)
                   <div (:id ,option-id
                         :dojoType #.+dijit/menu-item+
                         :iconClass ,class
                         :onClick `js-inline(wui.field.update-popup-menu-select-field ,div-id ,field-id ,possible-value ,class))
                     ,possible-value>))>)>))
