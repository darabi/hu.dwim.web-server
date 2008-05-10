;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(eval-when (:compile-toplevel :load-toplevel)
  (def (function eio) content-type-for (mime-type &optional (encoding (encoding-name-of *response*)))
    (or (case encoding
          (:utf-8    (switch (mime-type :test #'string=)
                       (+html-mime-type+ +utf-8-html-content-type+)
                       (+css-mime-type+  +utf-8-css-content-type+)))
          (:us-ascii (switch (mime-type :test #'string=)
                       (+html-mime-type+ +us-ascii-html-content-type+)
                       (+css-mime-type+  +us-ascii-css-content-type+)))
          (:iso8859-1 (switch (mime-type :test #'string=)
                        (+html-mime-type+ +iso-8859-1-html-content-type+)
                        (+css-mime-type+  +iso-8859-1-css-content-type+))))
        (concatenate 'string mime-type "; charset=" (string-downcase encoding)))))

(def (constant e :test 'string=) +html-content-type+ (content-type-for +html-mime-type+ +encoding+))
(def (constant e :test 'string=) +xml-content-type+ (content-type-for +xml-mime-type+ +encoding+))

(def (macro e) with-html-document-body ((&key title content-type) &body body)
  `<html
     <head
       <meta (:content ,(or ,content-type
                            (header-value *response* +header/content-type+))
              :http-equiv #.+header/content-type+)>
       <title ,,(or title "")>>
     <body ,,@body>>)

(def (macro e) with-request-params (request args &body body)
  "Bind, according the REQUEST-LAMBDA-LIST the parameters in
  REQUEST and execute BODY.

REQUEST-LAMBDA-LIST is a list of the form:

 ( [ ( symbol string ) | symbol ]
   [ default-value [ supplied-symbol-name ]? ]? )

If the request contains a param (no distinction between GET and
POST params is made) named STRING (which defaults to the symbol
name of SYMBOL) the variable SYMBOL is bound to the associated
value (which is always a string) . If no parameter with that name
was passed SYMBOL will be bound to DEFAULT-VALUE and the variable
named SUPPLIED-SYMBOL-NAME will be bound to NIL.

NB: Parameter names are matched case insensitively."
  (once-only (request)
    (bind (((:values bindings defaults)
            (iter (for entry :in args)
                  (setf entry (ensure-list entry))
                  (unless (and (< 0 (length entry) 5)
                               (not (null (first entry))))
                    (error "Invalid with-request-params entry ~S" entry))
                  (for name-part = (ensure-list (first entry)))
                  (unless (and (< 0 (length name-part) 3)
                               (not (null (first name-part))))
                    (error "Invalid with-request-params entry ~S" entry))
                  (for variable-name = (first name-part))
                  (for parameter-name = (second name-part))
                  (for default-value = (second entry))
                  (for supplied-variable-name = (or (third entry)
                                                    (gensym (concatenate-string
                                                             (string variable-name)
                                                             "-SUPPLIED?"))))
                  (unless parameter-name
                    (setf parameter-name (etypecase variable-name
                                             (symbol (string-downcase variable-name))
                                             (string variable-name))))
                  (collect `((:values ,variable-name ,supplied-variable-name)
                             (request-parameter-value ,parameter-name ,request)) :into bindings)
                  (collect `(unless ,supplied-variable-name
                              (setf ,variable-name ,default-value)) :into defaults)
                  (finally (return (values bindings defaults))))))
      `(bind ,bindings
         ,@defaults
         ,@body))))

