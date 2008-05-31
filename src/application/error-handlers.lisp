;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method handle-toplevel-condition ((application application) (error frame-out-of-sync-error))
  (bind ((refresh-uri (bind ((uri (clone-uri (uri-of *request*))))
                        (setf (uri-query-parameter-value uri +frame-index-parameter-name+) nil)
                        (setf (uri-query-parameter-value uri +action-id-parameter-name+) nil)
                        uri))
         (new-frame-uri (bind ((uri (clone-uri (uri-of *request*))))
                          (clear-uri-query-parameters uri)
                          (decorate-uri uri *application*)
                          (decorate-uri uri *session*)
                          uri)))
    (emit-simple-html-document-response (:status +http-not-acceptable+)
      <p "Browser window out of sync with the server...">
      <p "Please avoid using the back button of your "
         "browser and/or opening links in new windows by copy-pasting or using the \"Open in new window\" feature "
         "of your browser. To achieve the same effect, you can use the navigation actions provided by the application.">
      <p <a (:href ,(print-uri-to-string refresh-uri)) "Bring me back to the application">>
      <p <a (:href ,(print-uri-to-string new-frame-uri)) "Make this window another independent view of the application">>)))
