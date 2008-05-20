;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method handle-toplevel-condition ((application application) (error frame-out-of-sync-error))
  (bind ((uri (clone-uri (uri-of *request*))))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) nil)
    (bind ((uri-string (print-uri-to-string uri)))
      (emit-http-response ((+header/status+       +http-not-acceptable+
                            +header/content-type+ +html-content-type+))
        (with-html-document-body (:content-type +html-content-type+)
          <p "Frame out of sync... Please use the navigation actions provided by the application and avoid using the back button of your browser!">
          <p "Your view will be refreshed within a few seconds, or you can "
             <a (:href ,uri-string) "force the refresh now">
             " using the previous link.">)))))
