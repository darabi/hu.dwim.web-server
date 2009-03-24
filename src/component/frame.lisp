;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Frame

(def (constant e :test 'string=) +scroll-x-parameter-name+ "_sx")

(def (constant e :test 'string=) +scroll-y-parameter-name+ "_sy")

(def (constant e :test 'string=) +no-javascript-error-parameter-name+ "_njs")

(def (constant e :test 'string=) +page-failed-to-load-id+ "_failed-to-load")
(def (constant e) +page-failed-to-load-grace-period-in-millisecs+ 5000)

(def (constant e :test (constantly #t)) +mozilla-version-scanner+ (cl-ppcre:create-scanner "Mozilla/([0-9]{1,}\.[0-9]{0,})"))
(def (constant e :test (constantly #t)) +opera-version-scanner+ (cl-ppcre:create-scanner "Opera/([0-9]{1,}\.[0-9]{0,})"))

(def (constant e :test (constantly #t)) +msie-version-scanner+ (cl-ppcre:create-scanner "MSIE ([0-9]{1,}\.[0-9]{0,})"))

(def (constant e :test (constantly #t)) +drakma-version-scanner+ (cl-ppcre:create-scanner "Drakma/([0-9]{1,}\.[0-9]{0,})"))

(def (special-variable e) *dojo-skin-name* "tundra")
(def (special-variable e) *dojo-file-name* "dojo.js")
(def (special-variable e) *dojo-directory-name* "dojo/")

(def function find-latest-dojo-directory-name (wwwroot-directory)
  (bind ((dojo-dir (first (sort (remove-if [not (starts-with-subseq "dojo" !1)]
                                           (mapcar [last-elt (pathname-directory !1)]
                                                   (cl-fad:list-directory wwwroot-directory)))
                                #'string>=))))
    (assert dojo-dir () "Seems like there's not any dojo directory in ~S. Hint: see wui/etc/build-dojo.sh" wwwroot-directory)
    (concatenate-string dojo-dir "/")))

(def component frame-component (top-component layer-context-capturing-component-mixin)
  ((content-type +xhtml-content-type+)
   (stylesheet-uris nil)
   (script-uris nil)
   (page-icon nil)
   (title nil)
   (dojo-skin-name *dojo-skin-name*)
   (dojo-release-uri (parse-uri (concatenate-string "static/" *dojo-directory-name* "dojo/")))
   (dojo-file-name *dojo-file-name*)
   (parse-dojo-widgets-on-load #f :type boolean :accessor parse-dojo-widgets-on-load?)
   (debug-client-side *debug-client-side* :type boolean :accessor debug-client-side? :export :accessor)))

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
    <html (:xmlns     #.+xml-namespace-uri/xhtml+
           xmlns:dojo #.+xml-namespace-uri/dojo+)
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
        ,(bind (((icon-uri &optional file-name) (ensure-list (page-icon-of -self-))))
           (when icon-uri
             <link (:rel "icon"
                    :type "image/x-icon"
                    :href ,(append-file-write-date-to-uri (etypecase icon-uri
                                                            (string (concatenate-string path-prefix icon-uri))
                                                            (uri (prefix-uri-path (clone-uri icon-uri) path-prefix)))
                                                          file-name))>))
        <title ,(title-of -self-)>
        ,(foreach (lambda (el)
                    (bind (((stylesheet-uri &optional file-name) (ensure-list el)))
                      <link (:rel "stylesheet"
                             :type "text/css"
                             :href ,(append-file-write-date-to-uri (etypecase stylesheet-uri
                                                                     (string (concatenate-string path-prefix stylesheet-uri))
                                                                     (uri (prefix-uri-path (clone-uri stylesheet-uri) path-prefix)))
                                                                   file-name))>))
                  (stylesheet-uris-of -self-))
        <script (:type #.+javascript-mime-type+)
          ,(format nil "djConfig = { parseOnLoad: ~A, isDebug: ~A, locale: ~A }"
                   (to-js-boolean (parse-dojo-widgets-on-load? -self-))
                   (to-js-boolean debug-client-side?)
                   (to-js-literal (default-locale-of application)))>
        ;; TODO find out a nice way to get to the dojo.js file and append-file-write-date-to-uri
        <script (:type         #.+javascript-mime-type+
                 :src          ,(bind ((dojo-release-uri (dojo-release-uri-of -self-)))
                                  (concatenate-string (unless (host-of dojo-release-uri)
                                                        path-prefix)
                                                      (print-uri-to-string dojo-release-uri)
                                                      (dojo-file-name-of -self-)
                                                      (when debug-client-side?
                                                        ".uncompressed.js"))))
                 ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                 "">
        ,(foreach (lambda (el)
                    (bind (((script-uri &optional file-name) (ensure-list el)))
                      <script (:type         #.+javascript-mime-type+
                               :src          ,(append-file-write-date-to-uri (concatenate-string path-prefix script-uri) file-name))
                              ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                              "">))
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
             `js(setf window.location.href ,href)))
        <form (:method "post"
               :enctype #.+form-encoding/multipart-form-data+
               :action "")
          ;; KLUDGE not here, scroll stuff shouldn't be part of wui proper
          <div (:style "display: none")
            <input (:id #.+scroll-x-parameter-name+ :name #.+scroll-x-parameter-name+ :type "hidden"
                    :value ,(first (ensure-list (parameter-value +scroll-x-parameter-name+))))>
            <input (:id #.+scroll-y-parameter-name+ :name #.+scroll-y-parameter-name+ :type "hidden"
                    :value ,(first (ensure-list (parameter-value +scroll-y-parameter-name+))))>>
          ,(render-failed-to-load-page)
          `js(progn
               ;; don't use any non-standard js stuff here, because if things go wrong then nothing is guaranteed to be loaded...
               (defun _wui_handleFailedToLoad ()
                 (setf document.location.href document.location.href)
                 (return false))
               (bind ((failed-page (document.getElementById ,+page-failed-to-load-id+)))
                 (setf failed-page.style.display "none")
                 (setf document._wui_failed-to-load-timer (setTimeout (lambda ()
                                                                        ;; if things go wrong, at least have a timer that brings stuff back in the view
                                                                        (setf document.body.style.margin "0px")
                                                                        (setf failed-page.style.display ""))
                                                                      ,+page-failed-to-load-grace-period-in-millisecs+))))
          ,@(with-collapsed-js-scripts
             (with-dojo-widget-collector
               (render (content-of -self-)))
             `js(on-load
                 (log.debug "Clearing the failed to load timer")
                 (clearTimeout document._wui_failed-to-load-timer)
                 ;; KLUDGE: this should be done after, not only the page, but all widgets are loaded
                 (log.debug "Clearing the margin -10000px hackery")
                 (dojo.style document.body "margin" "0px")))>>>))

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
            (check +opera-version-scanner+ 9.6)
            (check +msie-version-scanner+ 7)
            (check +drakma-version-scanner+ 0))))))
