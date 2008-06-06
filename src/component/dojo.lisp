;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def special-variable *dojo-widget-ids*)

(macrolet ((x (&body entries)
             `(progn
                ,@(iter (for (name dojo-name) :on entries :by #'cddr)
                        (collect `(def (constant :test 'string=) ,name ,dojo-name))))))
  (x
   +dijit/date-text-box+         "dijit.form.DateTextBox"
   +dijit/simple-text-area+      "dijit.form.SimpleTextarea"
   +dijit/text-box+              "dijit.form.TextBox"
   +dijit/time-text-box+         "dijit.form.TimeTextBox"
   +dijit/number-text-box+       "dijit.form.NumberTextBox"
   ))

(def with-macro with-dojo-widget-collector ()
  (bind ((*dojo-widget-ids* nil))
    (multiple-value-prog1
        (-body-)
      (when *dojo-widget-ids*
        `js(on-load
            (dojo.parser.instantiate (map 'dojo.by-id (array ,@*dojo-widget-ids*))))))))

(def macro render-dojo-widget ((&optional (id '(generate-frame-unique-string "_w")))
                                &body body)
  (once-only (id)
    `(progn
       ,@body
       (push ,id *dojo-widget-ids*)
       (values)))
  ;; TODO could skip the ,@(list ...) if qq list quoting worked
  ;; TODO nees numerous qq fixes
  ;`<,,xml-element-name (:id ,,id
  ;                      dojo:name ,,name
  ;                      ,@(list ,@(iter (for (name value) :on attributes :by #'cddr)
  ;                                      (collect (make-xml-attribute name (make-xml-unquote value))))))>
  )
