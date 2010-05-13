;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Download file

(def (component e) download-file/widget (command/widget)
  ((directory "/tmp/")
   (file-name)
   (url-prefix "static/"))
  (:default-initargs :content (icon/widget download-file) :action nil))

(def (macro e) download-file/widget (&rest args &key &allow-other-keys)
  `(make-instance 'download-file/widget ,@args))

(def constructor (download-file/widget (label nil label?) &allow-other-keys) ()
  (when label?
    (setf (content-of -self-) (icon/widget download-file :label label))))

(def refresh-component download-file/widget
  (bind (((:slots file-name action url-prefix) -self-))
    (setf action (make-uri :path (string+ url-prefix (namestring file-name))))))

(def render-xhtml download-file/widget
  (bind ((absolute-file-name (merge-pathnames (file-name-of -self-) (directory-of -self-))))
    (unless (probe-file absolute-file-name)
      (setf (enabled-component? -self-) #f))
    <div (:class "download-file widget")
      ,(call-next-layered-method)
      " (" ,(file-last-modification-timestamp absolute-file-name) ")">))

;;;;;;
;;; Upload file

(def (component e) upload-file/widget (standard/widget)
  ((client-state-sink nil)))

(def (macro e) upload-file/widget ()
  '(make-instance 'upload-file/widget))

(def render-xhtml upload-file/widget
  (ensure-client-state-sink -self-)
  (render-upload-file-field :client-state-sink (client-state-sink-of -self-)))

;;;;;;
;;; Icon

(def (icon e) download-file)

(def (icon e) upload-file)
