;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; File download

(def (component e) file-download-component (command/widget)
  ((directory "/tmp/")
   (file-name)
   (url-prefix "static/"))
  (:default-initargs
     :content (icon download)
     :action nil))

(def constructor (file-download-component (label nil label?) &allow-other-keys) ()
  (when label?
    (setf (content-of -self-) (icon download :label label))))

(def refresh-component file-download-component
  (bind (((:slots file-name action url-prefix) -self-))
    (setf action (make-uri :path (string+ url-prefix (namestring file-name))))))

(def render-xhtml file-download-component
  (bind ((absolute-file-name (merge-pathnames (file-name-of -self-) (directory-of -self-))))
    (unless (probe-file absolute-file-name)
      (setf (enabled-component? -self-) #f))
    <div (:class "file-download")
         ,(call-next-method)
         " (" ,(file-last-modification-timestamp absolute-file-name) ")">))

;;;;;;
;;; File upload

(def (component e) file-upload-component (primitive-component)
  ())

(def render-xhtml file-upload-component
  (ensure-client-state-sink -self-)
  (render-file-upload-field :client-state-sink (client-state-sink-of -self-)))

(def method parse-component-value ((component file-upload-component) client-value)
  (etypecase client-value
    (string (values nil #t)) ; empty file upload field is posted from the browser
    (rfc2388-binary:mime-part client-value)))

;;;;;;
;;; Icon

(def (icon e) download)

(def (icon e) upload)