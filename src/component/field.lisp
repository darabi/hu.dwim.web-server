;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def function %emit-or-make-xml-attribute (name value)
  (cond
    ((delayed-processing? value)
     ;; this is kinda KLUDGEy: we must make sure we capture the output of any inline emitting done while force'ing the attribute value.
     ;; this piece of code relies on the actual setup of qq (namely: inline emitting into *html-stream*)
     ;; it feels like there must be a better solution to all this delay/force `js misery...
     (bind ((body (with-output-to-sequence (*html-stream*) :external-format +encoding+
                    (force value))))
       (unless (zerop (length body))
         (qq::write-quasi-quoted-binary
          (qq::transform-quasi-quoted-string-to-quasi-quoted-binary name :encoding +encoding+)
          *html-stream*)
         (write-sequence #.(babel:string-to-octets "=\"" :encoding +encoding+) *html-stream*)
         (write-sequence body *html-stream*)
         (write-sequence #.(babel:string-to-octets "\"" :encoding +encoding+) *html-stream*))
       nil))
    (value
     (make-xml-attribute name value))
    (t
     nil)))

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
          ;; :class (css-class field)
          ;; :style (css-style field)
          <input (:id ,id
                  :type "checkbox"
                  :checked ,checked
                  ,(%emit-or-make-xml-attribute "onChange" on-change))>
          `js(on-load
              (wui.field.setup-simple-checkbox ,id ,checked-tooltip ,unchecked-tooltip)))))
  (values))

;;;;;;
;;; String field

(def function render-string-field (type value client-state-sink &key (id (generate-frame-unique-string "_stw")) on-change on-key-down on-key-up)
  (render-dojo-widget (id)
    ;; TODO dojoRows 3
    <input (:dojoType #.+dijit/text-box+
            :type     ,type
            :id       ,id
            :name     ,(id-of client-state-sink)
            :value    ,value
            ,(%emit-or-make-xml-attribute "onChange" on-change)
            ,(%emit-or-make-xml-attribute "onKeyDown" on-key-down)
            ,(%emit-or-make-xml-attribute "onKeyUp" on-key-up))>))

;;;;;;
;;; Number field

(def function render-number-field (value client-state-sink &key (id (generate-frame-unique-string "_nrw")) on-change on-key-down on-key-up)
  (render-dojo-widget (id)
    <input (:dojoType #.+dijit/number-text-box+
            :type     "text"
            :id       ,id
            :name     ,(id-of client-state-sink)
            :value    ,value
            ,(%emit-or-make-xml-attribute "onChange" on-change)
            ,(%emit-or-make-xml-attribute "onKeyDown" on-key-down)
            ,(%emit-or-make-xml-attribute "onKeyUp" on-key-up))>))

;;;;;;
;;; Combo box

(def function render-combo-box-field (value possible-values &key (id (generate-frame-unique-string "_w")) name
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

(def function render-select-field (value possible-values &key (id (generate-frame-unique-string "_w")) name
                                         (key #'identity) (test #'equal) (client-name-generator #'princ-to-string) on-change)
  (render-dojo-widget (id)
    <select (:id ,id
             :dojoType #.+dijit/filtering-select+
             :name ,name
             ,(%emit-or-make-xml-attribute "onChange" on-change))
      ,(iter (for index :upfrom 0)
             (for possible-value :in-sequence possible-values)
             (for actual-value = (funcall key possible-value))
             (for client-name = (funcall client-name-generator actual-value))
             (for client-value = (integer-to-string index))
             (for selected = (when (funcall test value actual-value) "yes"))
             <option (:value ,client-value :selected ,selected) ,client-name>)>)
  (values))

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
