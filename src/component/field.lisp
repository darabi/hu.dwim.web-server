;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Checkbox field

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
