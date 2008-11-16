;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method handle-toplevel-condition :around ((application application) error)
  (if *ajax-aware-request*
      (emit-http-response ((+header/status+       +http-not-acceptable+
                            +header/content-type+ +xml-mime-type+))
        <ajax-response
         <error-message ,#"error-message-for-ajax-requests">
         <result "failure">>)
      (call-next-method)))

(def method handle-toplevel-condition ((application application) (error frame-out-of-sync-error))
  (bind ((refresh-uri (bind ((uri (clone-request-uri)))
                        (setf (uri-query-parameter-value uri +action-id-parameter-name+) nil)
                        (decorate-uri uri *frame*)
                        uri))
         (new-frame-uri (bind ((uri (clone-request-uri)))
                          (decorate-uri uri *application*)
                          (decorate-uri uri *session*)
                          (setf (uri-query-parameter-value uri +frame-id-parameter-name+) nil)
                          (setf (uri-query-parameter-value uri +frame-index-parameter-name+) nil)
                          uri)))
    (emit-simple-html-document-response (:status +http-not-acceptable+
                                         :headers #.(list 'quote +disallow-response-caching-header-values+))
      <p "Browser window out of sync with the server...">
      <p "Please avoid using the back button of your "
         "browser and/or opening links in new windows by copy-pasting or using the \"Open in new window\" feature "
         "of your browser. To achieve the same effect, you can use the navigation actions provided by the application.">
      <p <a (:href ,(print-uri-to-string refresh-uri)) "Bring me back to the application">>
      <p <a (:href ,(print-uri-to-string new-frame-uri)) "Make this window a new view of the application">>)))
