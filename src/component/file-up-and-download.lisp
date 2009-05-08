;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; File download

(def component file-download-component (command-component)
  ((directory "/tmp/")
   (file-name)
   (url-prefix "static/"))
  (:default-initargs
     :content (icon download)
     :action nil))

(def constructor (file-download-component (label nil label?) &allow-other-keys) ()
  (when label?
    (setf (content-of -self-) (icon download :label label))))

(def refresh file-download-component
  (bind (((:slots file-name action url-prefix) -self-))
    (setf action (make-uri :path (concatenate-string url-prefix (namestring file-name))))))

(def render-xhtml file-download-component
  (bind ((absolute-file-name (merge-pathnames (file-name-of -self-) (directory-of -self-))))
    (unless (probe-file absolute-file-name)
      (setf (enabled-p -self-) #f))
    <div (:class "file-download")
         ,(call-next-method)
         " (" ,(file-last-modification-timestamp absolute-file-name) ")">))


;;;;;;
;;; File upload

(def component file-upload-component (primitive-component)
  ())

(def render-xhtml file-upload-component
  (ensure-client-state-sink -self-)
  (render-file-upload-field :client-state-sink (client-state-sink-of -self-)))

(def method parse-component-value ((component file-upload-component) client-value)
  (etypecase client-value
    (string (values nil #t)) ; empty file upload field is posted from the browser
    (rfc2388-binary:mime-part client-value)))
