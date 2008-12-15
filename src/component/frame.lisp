;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Frame

(def (constant e :test 'string=) +scroll-x-parameter-name+ "_sx")

(def (constant e :test 'string=) +scroll-y-parameter-name+ "_sy")

(def (constant e :test 'string=) +no-javascript-error-parameter-name+ "_njs")

(def (constant e :test (constantly #t)) +mozilla-version-scanner+ (cl-ppcre:create-scanner "Mozilla/([0-9]{1,}[\.0-9]{0,})"))

(def (constant e :test (constantly #t)) +opera-version-scanner+ (cl-ppcre:create-scanner "Opera/([0-9]{1,}[\.0-9]{0,})"))

(def (constant e :test (constantly #t)) +msie-version-scanner+ (cl-ppcre:create-scanner "MSIE ([0-9]{1,}[\.0-9]{0,})"))

(def component frame-component (top-component layer-context-capturing-component-mixin)
  ((content-type +xhtml-content-type+)
   (stylesheet-uris nil)
   (script-uris nil)
   (page-icon nil)
   (title nil)
   (dojo-skin-name "tundra")
   (dojo-path "static/dojo/dojo/")
   (dojo-file-name "dojo.js")
   (parse-dojo-widgets-on-load #f :type boolean :accessor parse-dojo-widgets-on-load?)
   (debug-client-side (not *load-as-production-p*) :type boolean :accessor debug-client-side? :export :accessor)))

;; TODO support appending an &timestamp=12345678 to the urls to allow longer expires header and faster following of updates?
(def render frame-component ()
  (bind ((application *application*)
         (path-prefix (path-prefix-of application))
         (encoding (or (when *response*
                         (encoding-name-of *response*))
                       +encoding+))
         (debug-client-side? (debug-client-side? -self-))
         (no-javascript? (request-parameter-value *request* +no-javascript-error-parameter-name+)))
    (emit-xhtml-prologue encoding +xhtml-1.1-doctype+)
    <html (:xmlns     #.+xhtml-namespace-uri+
           xmlns:dojo #.+dojo-namespace-uri+)
      <head
        <meta (:http-equiv #.+header/content-type+
               :content ,(content-type-for +xhtml-mime-type+ encoding))>
        ,(unless no-javascript?
           <noscript <meta (:http-equiv #.+header/refresh+
                            :content ,(concatenate-string "0; URL="
                                                          (application-relative-path-for-no-javascript-support-error *application*)
                                                          "?"
                                                          +no-javascript-error-parameter-name+
                                                          "=t"))>>)
        ,(awhen (page-icon-of -self-)
           <link (:rel "icon"
                  :type "image/x-icon"
                  :href ,(concatenate-string path-prefix
                                             (etypecase it
                                               (string it)
                                               (uri (print-uri-to-string it)))))>)
        <title ,(title-of -self-)>
        ,(foreach (lambda (stylesheet-uri)
                    <link (:rel "stylesheet"
                                :type "text/css"
                                :href ,(concatenate-string path-prefix (etypecase stylesheet-uri
                                                                         (string stylesheet-uri)
                                                                         (uri (print-uri-to-string stylesheet-uri)))))>)
                  (stylesheet-uris-of -self-))
        <script (:type #.+javascript-mime-type+)
          ,(format nil "djConfig = { parseOnLoad: ~A, isDebug: ~A, locale: ~A }"
                   (to-js-boolean (parse-dojo-widgets-on-load? -self-))
                   (to-js-boolean debug-client-side?)
                   (to-js-literal (default-locale-of application)))>
        <script (:type         #.+javascript-mime-type+
                 :src          ,(concatenate-string path-prefix
                                                    (dojo-path-of -self-)
                                                    (dojo-file-name-of -self-)
                                                    (when debug-client-side?
                                                      ".uncompressed.js")))
                 ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                 "">
        ,(foreach (lambda (script-uri)
                    <script (:type         #.+javascript-mime-type+
                             :src          ,(concatenate-string path-prefix script-uri))
                            ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                            "">)
                  (script-uris-of -self-))>
      <body (:class ,(dojo-skin-name-of -self-)
             :style ,(unless no-javascript? "margin-left: -10000px;"))
        `js(on-load
            ;; KLUDGE not here, scroll stuff shouldn't be part of wui proper
            (wui.reset-scroll-position "content")
            (setf wui.session-id  ,(or (awhen *session* (id-of it)) ""))
            (setf wui.frame-id    ,(or (awhen *frame* (id-of it)) ""))
            (setf wui.frame-index ,(or (awhen *frame* (frame-index-of it)) "")))
        ;; NOTE: if there's javascript turned on just reload without parameter (this might be true after enabling it and pressing refresh)
        ,(when no-javascript?
           (bind ((href (print-uri-to-string (clone-request-uri :strip-query-parameters (list +no-javascript-error-parameter-name+)))))
             `js(on-load (wui.io.action ,href #f #f))))
        <form (:method "post"
               :action "")
          ;; KLUDGE not here, scroll stuff shouldn't be part of wui proper
          <div (:style "display: none")
            <input (:id #.+scroll-x-parameter-name+ :name #.+scroll-x-parameter-name+ :type "hidden"
                    :value ,(first (ensure-list (request-parameter-value *request* +scroll-x-parameter-name+))))>
            <input (:id #.+scroll-y-parameter-name+ :name #.+scroll-y-parameter-name+ :type "hidden"
                    :value ,(first (ensure-list (request-parameter-value *request* +scroll-y-parameter-name+))))>>
          ,@(with-collapsed-js-scripts
             (with-dojo-widget-collector
               (render (content-of -self-))))>>>))

(def (macro e) frame ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'frame-component ,@args :content ,(the-only-element content)))

(def (generic e) application-relative-path-for-no-javascript-support-error (application)
  (:method ((application application))
    "/help/"))

(def (generic e) supported-user-agent? (application request)
  (:method ((application application) (request request))
    (bind ((http-agent (header-value request +header/user-agent+)))
      (flet ((check (version-scanner minimum-version)
               (bind (((:values success? version) (cl-ppcre:scan-to-strings version-scanner http-agent)))
                 (and success?
                      (<= minimum-version (parse-number:parse-number (first-elt version)))))))
        (or (check +mozilla-version-scanner+ 5)
            (check +opera-version-scanner+ 9)
            (check +msie-version-scanner+ 7))))))
