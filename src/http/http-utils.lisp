;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(eval-when (:compile-toplevel :load-toplevel)
  (def (function eio) content-type-for (mime-type &optional (encoding (encoding-name-of *response*)))
    ;; this is a somewhat ugly optimization: return constants for the most often used combinations
    (or (case encoding
          (:utf-8    (case mime-type
                       (+html-mime-type+  +utf-8-html-content-type+)
                       (+xhtml-mime-type+ +utf-8-xhtml-content-type+)
                       (+css-mime-type+   +utf-8-css-content-type+)
                       (t (switch (mime-type :test #'string=)
                            (+html-mime-type+  +utf-8-html-content-type+)
                            (+xhtml-mime-type+ +utf-8-xhtml-content-type+)
                            (+css-mime-type+   +utf-8-css-content-type+)))))
          (:us-ascii (case mime-type
                       (+html-mime-type+  +us-ascii-html-content-type+)
                       (+xhtml-mime-type+ +us-ascii-xhtml-content-type+)
                       (+css-mime-type+   +us-ascii-css-content-type+)
                       (t (switch (mime-type :test #'string=)
                            (+html-mime-type+  +us-ascii-html-content-type+)
                            (+xhtml-mime-type+ +us-ascii-xhtml-content-type+)
                            (+css-mime-type+   +us-ascii-css-content-type+)))))
          (:iso8859-1 (case mime-type
                        (+html-mime-type+  +iso-8859-1-html-content-type+)
                        (+xhtml-mime-type+ +iso-8859-1-xhtml-content-type+)
                        (+css-mime-type+   +iso-8859-1-css-content-type+)
                        (t (switch (mime-type :test #'string=)
                             (+html-mime-type+  +iso-8859-1-html-content-type+)
                             (+xhtml-mime-type+ +iso-8859-1-xhtml-content-type+)
                             (+css-mime-type+   +iso-8859-1-css-content-type+))))))
        (concatenate 'string mime-type "; charset=" (string-downcase encoding)))))

(def (constant e :test 'string=) +html-content-type+  (content-type-for +html-mime-type+  +encoding+))
(def (constant e :test 'string=) +xhtml-content-type+ (content-type-for +xhtml-mime-type+ +encoding+))
(def (constant e :test 'string=) +xml-content-type+   (content-type-for +xml-mime-type+   +encoding+))

(def (constant e :test (constantly #t)) +xml-attribute/xhtml-xmlns+
    (make-xml-attribute "xmlns" +xhtml-namespace-uri+))
(def (constant e :test (constantly #t)) +xml-attribute/dojo-xmlns+
    (make-xml-attribute "xmlns:dojo" +dojo-namespace-uri+))

(def (with-macro* e) with-html-document (&key title
                                              content-type
                                              encoding
                                              (html-tag-attributes '(#.+xml-attribute/xhtml-xmlns+
                                                                     #.+xml-attribute/dojo-xmlns+))
                                              (xhtml-doctype +xhtml-1.1-doctype+ xhtml-doctype-provided?)
                                              page-icon
                                              stylesheet-uris)
  (bind ((response (when (boundp '*response*)
                     *response*))
         (encoding (or encoding
                       (when response
                         (encoding-name-of response))
                       +encoding+))
         (content-type (or content-type
                           (when response
                             (header-value response +header/content-type+)))))
    (unless xhtml-doctype-provided?
      ;; TODO gracefully fall back to plain html if the request is coming from a crippled browser
      )
    (when xhtml-doctype
      (write-string "<?xml version=\"1.1\" encoding=\"" *html-stream*)
      (write-string (string encoding) *html-stream*)
      (write-string "\"?>" *html-stream*)
      (write-char #\Newline *html-stream*)
      (write-string "<!DOCTYPE html PUBLIC " *html-stream*)
      (write-string xhtml-doctype *html-stream*)
      (write-char #\> *html-stream*)
      (write-char #\Newline *html-stream*))
    <html (,@html-tag-attributes)
     <head
      ,(when content-type
         <meta (:http-equiv #.+header/content-type+ :content ,content-type)>)
      ,(when page-icon
         <link (:rel "icon" :type "image/x-icon" :href ,page-icon)>)
      <title ,title>
      ,@(mapcar (lambda (stylesheet-uri)
                  <link (:rel "stylesheet" :type "text/css"
                         :href ,(if (stringp stylesheet-uri)
                                    (escape-as-uri stylesheet-uri)
                                    (print-uri-to-string stylesheet-uri)))>)
                stylesheet-uris)>
     <body ,@(with-collapsed-js-scripts
              (list (-body-)))>>))

#||
;; TODO
(def (macro e) with-xhtml-document ((&key title content-type) &body body)
  `(progn
     (format *html-stream* "<?xml version=\"1.0\" encoding=\"~A\"?>" (encoding-name-of *response*))
     <html
       <head
         <meta (:content ,(or ,content-type
                              (header-value *response* +header/content-type+))
                :http-equiv #.+header/content-type+)>
         <title ,,(or title "")>>
       <body ,,@body>>))
||#

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
named SUPPLIED-SYMBOL-NAME will be bound to NIL."
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
                             (request-parameter-value ,request ,parameter-name)) :into bindings)
                  (collect `(unless ,supplied-variable-name
                              (setf ,variable-name ,default-value)) :into defaults)
                  (finally (return (values bindings defaults))))))
      `(bind ,bindings
         ,request ;; suppress the warning
         ,@defaults
         ,@body))))

(def (function eio) make-cookie (name value &key comment domain max-age path secure)
  (rfc2109:make-cookie
   :name name
   :value (escape-as-uri value)
   :comment comment
   :domain domain
   :max-age max-age
   :path (awhen path
           (escape-as-uri it))
   :secure secure))
