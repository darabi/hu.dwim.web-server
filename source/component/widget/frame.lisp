;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; frame/widget

(def (component e) frame/widget (top/abstract layer/mixin)
  ((content-mime-type +xhtml-mime-type+)
   (stylesheet-uris nil)
   (script-uris nil)
   (page-icon-uri nil)
   (title nil)
   (dojo-skin-name *dojo-skin-name*)
   (dojo-release-uri (parse-uri (string+ "static/dojo/" *dojo-directory-name* "dojo/")))
   (dojo-file-name *dojo-file-name*)
   (parse-dojo-widgets-on-load #f :type boolean)
   (debug-client-side *debug-client-side* :type boolean)))

(def (macro e) frame/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'frame/widget ,@args :content ,(the-only-element content)))

(def method parent-component-of ((self frame/widget))
  nil)

(def render-xhtml frame/widget
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
               :content ,(content-type-for (content-mime-type-of -self-) encoding))>
        ,(bind (((icon-uri &optional file-name) (ensure-list (page-icon-uri-of -self-))))
           (when icon-uri
             <link (:rel "icon"
                    :type "image/x-icon"
                    :href ,(append-file-write-date-to-uri (etypecase icon-uri
                                                            (string (string+ path-prefix icon-uri))
                                                            (uri (prefix-uri-path (clone-uri icon-uri) path-prefix)))
                                                          +timestamp-parameter-name+
                                                          file-name))>))
        <title ,(title-of -self-)>
        ,(foreach (lambda (el)
                    (bind (((stylesheet-uri &optional file-name) (ensure-list el)))
                      <link (:rel "stylesheet"
                             :type "text/css"
                             :href ,(append-file-write-date-to-uri (etypecase stylesheet-uri
                                                                     (string (string+ path-prefix stylesheet-uri))
                                                                     (uri (prefix-uri-path (clone-uri stylesheet-uri) path-prefix)))
                                                                   +timestamp-parameter-name+
                                                                   file-name))>))
                  (stylesheet-uris-of -self-))
        <script (:type #.+javascript-mime-type+)
          ,(format nil "djConfig = { parseOnLoad: ~A, isDebug: ~A, debugAtAllCosts: ~A, locale: ~A }"
                   (to-js-boolean (parse-dojo-widgets-on-load? -self-))
                   (to-js-boolean debug-client-side?)
                   (to-js-boolean debug-client-side?)
                   (to-js-literal (default-locale-of application)))>
        <script (:type         #.+javascript-mime-type+
                 :src          ,(bind ((dojo-release-uri (dojo-release-uri-of -self-)))
                                  (string+ (unless (host-of dojo-release-uri)
                                             path-prefix)
                                           (print-uri-to-string dojo-release-uri)
                                           (dojo-file-name-of -self-)
                                           (when debug-client-side?
                                             ".uncompressed.js"))))
                 ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                 "">
        ,(foreach (lambda (el)
                    (bind (((script-url &optional file-name) (ensure-list el)))
                      <script (:type         #.+javascript-mime-type+
                               :src          ,(bind ((url script-url))
                                                (unless (starts-with #\/ url)
                                                  (setf url (string+ path-prefix url)))
                                                (append-file-write-date-to-uri url +timestamp-parameter-name+ file-name)))
                              ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                              "">))
                  (script-uris-of -self-))>
      <body (:class ,(dojo-skin-name-of -self-)
             :style ,(when javascript-supported? "margin-left: -10000px;"))
        ;; TODO: this causes problems when content-type is application/xhtml+xml
        ;;       should solve the no javascript issue in a different way
        ,(when javascript-supported?
           <noscript <meta (:http-equiv #.+header/refresh+
                            :content ,(string+ "0; URL="
                                               (application-relative-path-for-no-javascript-support-error *application*)
                                               "?"
                                               +no-javascript-error-parameter-name+
                                               "=t"))>>
           (apply-localization-function 'render-failed-to-load-page)
           `js-xml(progn
                    ;; don't use any non-standard js stuff for the failed-to-load machinery, because if things go wrong then nothing is guaranteed to be loaded...
                    (defun _wui_handleFailedToLoad ()
                      (setf document.location.href document.location.href)
                      (return false))
                    (bind ((failed-page (document.getElementById ,+page-failed-to-load-id+)))
                      (setf failed-page.style.display "none")
                      (setf document.wui-failed-to-load-timer (setTimeout (lambda ()
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
                     (setf wui.frame-index ,(or (awhen *frame* (frame-index-of it)) "")))))
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
               (render-content-for -self-))
             `js(on-load
                 (log.debug "Clearing the failed to load timer")
                 (clearTimeout document.wui-failed-to-load-timer)
                 ;; KLUDGE: this should be done after, not only the page, but all widgets are loaded
                 (log.debug "Clearing the margin -10000px hackery")
                 (dojo.style document.body "margin" "0px")))>>>))

(def method supports-debug-component-hierarchy? ((self frame/widget))
  #f)

(def (generic e) application-relative-path-for-no-javascript-support-error (application)
  (:method ((application application))
    "/help/"))

(def (function e) make-default-page-icon-uri (system-name &optional (path "wui/image/miscellaneous/favicon.ico"))
  (list (string+ "static/" path) (system-relative-pathname system-name (string+ "www/" path))))

(def (function e) make-default-script-uris (system-name &rest script-uris)
  (declare (ignore system-name))
  (list* "wui/js/wui.js"
         +js-i18n-broker/default-path+
         +js-component-hierarchy-serving-broker/default-path+
         script-uris))

(def (function e) make-default-stylesheet-uris (system-name &rest style-sheets)
  (flet ((entry (path)
           (list (string+ "static/" path)
                 (system-relative-pathname system-name (string+ "www/" path))))
         (dojo-relative-path (path)
           (string+ "static/dojo/" *dojo-directory-name* path)))
    (list* (dojo-relative-path "dojo/resources/dojo.css")
           (dojo-relative-path "dijit/themes/tundra/tundra.css")
           (entry "wui/css/wui.css")
           (entry "wui/css/icon.css")
           (entry "wui/css/border.css")
           (entry "wui/css/layout.css")
           (entry "wui/css/widget.css")
           (entry "wui/css/text.css")
           (entry "wui/css/lisp-form.css")
           (entry "wui/css/shell-script.css")
           (entry "wui/css/presentation.css")
           (mapcar #'entry style-sheets))))
