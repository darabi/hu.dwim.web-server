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

(def method refresh-component ((self file-download-component))
  (with-slots (file-name action url-prefix) self
    (setf action (make-uri :path (concatenate-string url-prefix (namestring file-name))))))

(def render file-download-component ()
  (bind ((absolute-file-name (merge-pathnames (file-name-of -self-) (directory-of -self-))))
    (unless (probe-file absolute-file-name)
      (setf (enabled-p -self-) #f))
    <div (:class "file-download")
         ,(call-next-method)
         " (" ,(file-last-modification-timestamp absolute-file-name) ")">))


;;;;;;
;;; File upload

(def component file-upload-component (command-component)
  ((content (icon upload) :type component)
   (directory "/tmp/")
   (file-name)))

(def render file-upload-component ()
  (bind (((:read-only-slots icon) -self-))
    <div ,(render icon)
         <div <input (:type "file")>>>))
