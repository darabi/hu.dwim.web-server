;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Frame

(def (component ea) frame/basic (top/abstract layer-context-capturing/mixin)
  ((content-type +xhtml-content-type+)
   (stylesheet-uris nil)
   (script-uris nil)
   (page-icon nil)
   (title nil)
   (dojo-skin-name *dojo-skin-name*)
   (dojo-release-uri (parse-uri (concatenate-string "static/" *dojo-directory-name* "dojo/")))
   (dojo-file-name *dojo-file-name*)
   (parse-dojo-widgets-on-load #f :type boolean)
   (debug-client-side *debug-client-side* :type boolean)))

(def (macro e) frame ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'frame/basic ,@args :content ,(the-only-element content)))

(def render-xhtml frame/basic
  (bind ((application *application*)
         (path-prefix (path-prefix-of application))
         (encoding (or (when *response*
                         (encoding-name-of *response*))
                       +default-encoding+))
         (debug-client-side? (debug-client-side? -self-))
         (javascript-supported? (not (request-parameter-value *request* +no-javascript-error-parameter-name+))))
    (emit-xhtml-prologue encoding +xhtml-1.1-doctype+)
    <html (:xmlns     #.+xml-namespace-uri/xhtml+
           xmlns:dojo #.+xml-namespace-uri/dojo+)
      <head
        <meta (:http-equiv #.+header/content-type+
               :content ,(content-type-for +xhtml-mime-type+ encoding))>
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
             :style ,(when javascript-supported? "margin-left: -10000px;"))
        ,(when javascript-supported?
           <noscript <meta (:http-equiv #.+header/refresh+
                            :content ,(concatenate-string "0; URL="
                                                          (application-relative-path-for-no-javascript-support-error *application*)
                                                          "?"
                                                          +no-javascript-error-parameter-name+
                                                          "=t"))>>
           (apply-resource-function 'render-failed-to-load-page))
        `js-xml(progn
                 ;; don't use any non-standard js stuff for the failed-to-load machinery, because if things go wrong then nothing is guaranteed to be loaded...
                 (defun _wui_handleFailedToLoad ()
                   (setf document.location.href document.location.href)
                   (return false))
                 (bind ((failed-page (document.getElementById ,+page-failed-to-load-id+)))
                   (setf failed-page.style.display "none")
                   (setf document._wui_failed-to-load-timer (setTimeout (lambda ()
                                                                          ;; if things go wrong, at least have a timer that brings stuff back in the view
                                                                          (setf document.body.style.margin "0px")
                                                                          (dolist (child document.body.childNodes)
                                                                            (when child.style
                                                                              (setf child.style.display "none")))
                                                                          (setf failed-page.style.display ""))
                                                                        ,+page-failed-to-load-grace-period-in-millisecs+)))
                 (on-load
                  ;; KLUDGE not here, scroll stuff shouldn't be part of wui proper
                  (wui.reset-scroll-position "content")
                  (setf wui.session-id  ,(or (awhen *session* (id-of it)) ""))
                  (setf wui.frame-id    ,(or (awhen *frame* (id-of it)) ""))
                  (setf wui.frame-index ,(or (awhen *frame* (frame-index-of it)) ""))))
        ;; NOTE: if javascript is turned on in the browser, then just reload without the marker parameter (this might be true after enabling it and pressing refresh)
        ,(unless javascript-supported?
           (bind ((href (print-uri-to-string (clone-request-uri :strip-query-parameters (list +no-javascript-error-parameter-name+)))))
             `js-xml(setf window.location.href ,href)))
        <form (:method "post"
               :enctype #.+form-encoding/multipart-form-data+
               :action "")
          ;; KLUDGE not here, scroll stuff shouldn't be part of wui proper
          <div (:style "display: none")
            <input (:id #.+scroll-x-parameter-name+ :name #.+scroll-x-parameter-name+ :type "hidden"
                    :value ,(first (ensure-list (parameter-value +scroll-x-parameter-name+))))>
            <input (:id #.+scroll-y-parameter-name+ :name #.+scroll-y-parameter-name+ :type "hidden"
                    :value ,(first (ensure-list (parameter-value +scroll-y-parameter-name+))))>>
          ,@(with-collapsed-js-scripts
             (with-dojo-widget-collector
               (render-component (content-of -self-)))
             `js(on-load
                 (log.debug "Clearing the failed to load timer")
                 (clearTimeout document._wui_failed-to-load-timer)
                 ;; KLUDGE: this should be done after, not only the page, but all widgets are loaded
                 (log.debug "Clearing the margin -10000px hackery")
                 (dojo.style document.body "margin" "0px")))>>>))

(def method supports-debug-component-hierarchy? ((self frame/basic))
  #f)

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
