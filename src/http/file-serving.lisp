;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* file-serving-broker (broker-with-url-prefix)
  ((root-directory))
  (:metaclass closer-mop:funcallable-standard-class))

(def (function e) make-file-serving-broker (url-prefix root-directory)
  (make-instance 'file-serving-broker :url-prefix url-prefix :root-directory root-directory))

(def constructor file-serving-broker
  (closer-mop:set-funcallable-instance-function
    self
    (lambda (request)
      (file-serving-handler request (root-directory-of self) (url-prefix-of self)))))

(def function file-serving-handler (request root-directory url-prefix)
  (bind (((:values matches? relative-path) (matches-url-prefix? url-prefix request)))
    (when matches?
      (file-serving-response-for-query-path url-prefix relative-path root-directory))))

(def function file-serving-response-for-query-path (url-prefix relative-path root-directory)
  ;; TODO this function could be cached
  (bind ((pathname (merge-pathnames relative-path root-directory))
         (truename (ignore-errors
                     (probe-file pathname))))
    (when truename
      (cond
        ((cl-fad:directory-pathname-p truename)
         (make-directory-index-response url-prefix relative-path root-directory truename))
        ((not (null (pathname-name pathname)))
         (make-file-serving-response truename))))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file serving response

(def class* file-serving-response (response)
  ((file-name)))

(def (function e) make-file-serving-response (file-name)
  (make-instance 'file-serving-response :file-name file-name))

(defmethod send-response ((self file-serving-response))
  (serve-file (file-name-of self)))


;;;;;;;;;;;;;;;;;;;
;;; directory index

(def class* directory-index-response (response)
  ((url-prefix)
   (root-directory)
   (relative-path)
   (directory)))

(def (function e) make-directory-index-response (url-prefix relative-path root-directory
                                                 &optional (directory (merge-pathnames relative-path root-directory)))
  (aprog1
      (make-instance 'directory-index-response
                     :url-prefix url-prefix :root-directory root-directory
                     :relative-path relative-path :directory directory)
    (setf (header-value it +header/content-type+) +utf-8-html-content-type+)))

(defmethod send-response ((self directory-index-response))
  (call-next-method)
  (emit-into-html-stream (network-stream-of *request*)
    (with-simple-html-body (:title "foo")
      <table
        ,@(bind ((elements (cl-fad:list-directory (directory-of self)))
                 (url-prefix (url-prefix-of self))
                 (relative-path (relative-path-of self))
                 ((:values directories files)
                  (iter (for element :in elements)
                        (if (cl-fad:directory-pathname-p element)
                            (collect element :into dirs)
                            (collect element :into files))
                        (finally (return (values dirs files))))))
            (iter (for directory :in directories)
                  (for name = (lastcar (pathname-directory directory)))
                  <tr
                    <td
                      <a (:href ,(concatenate-string url-prefix relative-path name "/"))
                        ,name "/">>>)
            (iter (for file :in files)
                  (for name = (apply 'concatenate-string
                                     (pathname-name file)
                                     (awhen (pathname-type file)
                                       (list "." it))))
                  <tr
                    <td
                      <a (:href ,(concatenate-string url-prefix relative-path name))
                        ,name>>
                    <td ,(princ-to-string (nix:stat-size (nix:stat (namestring file))))>>))>)))

;;;;;;;;;;;;;;;;;;;
;;; MIME stuff for serving static files

(defparameter *mime-type->extensions* nil)
(defparameter *extension->mime-types* nil)
(defvar *mime-types-file* #P"/etc/mime.types")

(def function ensure-mime-types-are-read ()
  (when (and (null *mime-type->extensions*)
             (probe-file *mime-types-file*))
    (read-mime-types-file *mime-types-file*)))

(defun parse-mime-types-file (mime-types-file visitor)
  "Parser for /etc/mime.types"
  (iter
    (for line :in-file mime-types-file :using #'read-line)
    (when (or (string= "" line)
              (eq #\# (aref line 0)))
      (next-iteration))
    (for pieces = (remove-if (lambda (piece)
                               (zerop (length piece)))
                             (cl-ppcre:split "( |	)" line)))
    (unless (null pieces)
      (funcall visitor pieces))))

(defun read-mime-types-file (mime-types-file)
  "Read in /etc/mime.types file."
  (setf *mime-type->extensions* (make-hash-table :test #'equal))
  (setf *extension->mime-types* (make-hash-table :test #'equal))
  (parse-mime-types-file
   mime-types-file
   (lambda (pieces)
     (bind (((type &rest extensions) pieces))
       (setf (gethash type *mime-type->extensions*)
             (append extensions
                     (gethash type *mime-type->extensions*)))
       (dolist (extension extensions)
         (setf (gethash extension *extension->mime-types*)
               (list* type (gethash extension *extension->mime-types*))))))))

(defun extensions-for-mime-type (mime-type)
  "Extensions that can be given to file of given MIME type."
  (declare (type string mime-type))
  (gethash mime-type *mime-type->extensions*))

(defun mime-types-for-extension (extension)
  "MIME types associated with the given file extension."
  (declare (type string extension))
  (gethash extension *extension->mime-types*))

(with-simple-restart (continue "Ignore the error and continue without reading ~A" *mime-types-file*)
  (ensure-mime-types-are-read))
