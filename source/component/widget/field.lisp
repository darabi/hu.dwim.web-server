;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Checkbox field

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
          ;; :class (style-class field)
          <div (:id ,id)
            ,(if (and checked-image
                      unchecked-image)
                 <img>)>
          `js(on-load
              (wui.field.setup-custom-checkbox ,id ,checked-image ,unchecked-image ,checked-tooltip ,unchecked-tooltip ,checked-class ,unchecked-class)))
        (progn
          ;; TODO :accesskey (accesskey field)
          ;; :title (or (tooltip field) (if value
          ;;                                (enabled-tooltip-of field)
          ;;                                (disabled-tooltip-of field)))
          ;; :tabindex (tabindex field)
          ;; :class (style-class field)
          ;; :style (style field)
          <input (:id ,id
                  :type "checkbox"
                  :checked ,checked
                  ,(maybe-make-xml-attribute "onChange" on-change))>
          `js(on-load
              (wui.field.setup-simple-checkbox ,id ,checked-tooltip ,unchecked-tooltip)))))
  (values))

;;;;;;
;;; String field

(def function render-string-field (type value client-state-sink &key (id (generate-unique-string/frame "_stw")) on-change on-key-down on-key-up)
  (render-dojo-widget (id)
    ;; TODO dojoRows 3
    <input (:dojoType #.+dijit/text-box+
            :type     ,type
            :id       ,id
            :name     ,(id-of client-state-sink)
            :value    ,value
            ,(maybe-make-xml-attribute "onChange" on-change)
            ,(maybe-make-xml-attribute "onKeyDown" on-key-down)
            ,(maybe-make-xml-attribute "onKeyUp" on-key-up))>))

;;;;;;
;;; Number field

(def function render-number-field (value client-state-sink &key (id (generate-unique-string/frame "_nrw")) on-change on-key-down on-key-up)
  (render-dojo-widget (id)
    <input (:dojoType #.+dijit/number-text-box+
            :type     "text"
            :id       ,id
            :name     ,(id-of client-state-sink)
            :value    ,value
            ,(maybe-make-xml-attribute "onChange" on-change)
            ,(maybe-make-xml-attribute "onKeyDown" on-key-down)
            ,(maybe-make-xml-attribute "onKeyUp" on-key-up))>))

;;;;;;
;;; Combo box

(def function render-combo-box-field (value possible-values &key (id (generate-unique-string/frame "_w")) name
                                            (key #'identity) (test #'equal) (client-name-generator #'princ-to-string))
  <select (:id ,id
           :name ,name)
    ,(iter (for index :upfrom 0)
           (for possible-value :in-sequence possible-values)
           (for actual-value = (funcall key possible-value))
           (for client-name = (funcall client-name-generator actual-value))
           (for client-value = (integer-to-string index))
           (for selected = (when (funcall test value actual-value) "yes"))
           <option (:value ,client-value :selected ,selected)
                   ,client-name>)>)

;;;;;;
;;; Select field

(def function render-select-field (value possible-values &key (id (generate-unique-string/frame "_w")) name
                                         (key #'identity) (test #'equal) (client-name-generator #'princ-to-string) on-change)
  (render-dojo-widget (id)
    <select (:id ,id
             :dojoType #.+dijit/filtering-select+
             :name ,name
             ,(maybe-make-xml-attribute "onChange" on-change))
      ;; TODO the qq patch "xml: add optimizations that collapse xml tags even when a macro and list qq is also involved" breaks this
      ;; add a qq test that triggers this
      ,(iter (for index :upfrom 0)
             (for possible-value :in-sequence possible-values)
             (for actual-value = (funcall key possible-value))
             (for client-name = (funcall client-name-generator actual-value))
             (for client-value = (integer-to-string index))
             (for selected = (when (funcall test value actual-value) "yes"))
             <option (:value ,client-value :selected ,selected) ,client-name>)>)
  (values))

;;;;;;
;;; Upload file

(def function render-upload-file-field (&key (id (generate-unique-string/frame)) access-key tooltip tab-index
                                             class style client-state-sink (name (awhen client-state-sink (id-of it))))
  <input (:id ,id
          :class ,class
          :style ,style
          :name ,name
          :accesskey ,access-key
          :type "file"
          :title ,tooltip
          :tabindex ,tab-index)>)

;;;;;;
;;; Popup menu select field

(def function render-popup-menu-select-field (value possible-values &key value-sink classes (test #'equal) (key #'identity))
  (bind ((div-id (generate-unique-string/frame))
         (menu-id (generate-unique-string/frame))
         (field-id (generate-unique-string/frame))
         (name (id-of (client-state-sink (client-value)
                        (funcall value-sink client-value))))
         (index (position value possible-values :key key :test test)))
    <input (:id ,field-id :type "hidden" :name ,name :value ,value)>
    <div (:id ,div-id :class ,(nth index classes))
      ,(unless classes
         value)
      ,(render-dojo-widget (menu-id)
        <div (:id ,menu-id
              :class "wuiComboBox"
              :dojoType #.+dijit/menu+
              :leftClickToOpen "true"
              :style "display: none;"
              :targetNodeIds ,div-id)
          ,(iter (for possible-value :in possible-values)
                 (for class :in classes)
                 (for option-id = (generate-unique-string/frame))
                 (render-dojo-widget (option-id)
                   <div (:id ,option-id
                         :dojoType #.+dijit/menu-item+
                         :iconClass ,class
                         :onClick `js-inline(wui.field.update-popup-menu-select-field ,div-id ,field-id ,possible-value ,class))
                     ,possible-value>))>)>))
