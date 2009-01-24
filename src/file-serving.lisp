;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* file-serving-broker/cache-entry ()
  ((file-path)
   (file-write-date)
   (bytes-to-respond)
   (last-updated-at (get-monotonic-time))
   (last-used-at (get-monotonic-time))))

(def function make-file-serving-broker/cache-entry (file-path &optional file-write-date bytes-to-respond)
  (make-instance 'file-serving-broker/cache-entry
                 :file-path file-path
                 :file-write-date file-write-date
                 :bytes-to-respond bytes-to-respond))

(def class* file-serving-broker (broker-with-path-prefix)
  ((root-directory)
   ;; TODO (files-only #f)
   (file-path->cache-entry (make-hash-table :test 'equal)))
  (:metaclass funcallable-standard-class))

;; TODO caching

(def (function e) make-file-serving-broker (path-prefix root-directory &key priority)
  (make-instance 'file-serving-broker :path-prefix path-prefix :root-directory root-directory :priority priority))

(def constructor file-serving-broker
  (set-funcallable-instance-function
    -self-
    (lambda (request)
      (file-serving-handler -self- request (root-directory-of -self-) (path-prefix-of -self-)))))

(def function file-serving-handler (broker request root-directory path-prefix)
  (bind (((:values matches? relative-path) (request-uri-matches-path-prefix? path-prefix request)))
    (when matches?
      (server.debug "Returning file serving response for path-prefix ~S, relative-path ~S, root-directory ~A" path-prefix relative-path root-directory)
      (make-file-serving-response-for-query-path broker path-prefix relative-path root-directory))))

(def generic make-file-serving-response-for-query-path (broker path-prefix relative-path root-directory)
  (:method ((broker file-serving-broker) path-prefix relative-path root-directory)
    (when (or (zerop (length relative-path))
              (alphanumericp (elt relative-path 0)))
      (bind ((pathname (merge-pathnames relative-path root-directory))
             (truename (ignore-errors
                         (probe-file pathname))))
        (when truename
          (make-file-serving-response-for-directory-entry broker truename path-prefix relative-path root-directory))))))

(def generic make-file-serving-response-for-directory-entry (broker truename path-prefix relative-path root-directory)
  (:method ((broker file-serving-broker) truename path-prefix relative-path root-directory)
    (cond
      ((cl-fad:directory-pathname-p truename)
       (if (or (ends-with #\/ relative-path)
               (zerop (length relative-path)))
           (make-directory-index-response path-prefix relative-path root-directory truename)
           (bind ((uri (clone-request-uri)))
             (make-redirect-response (append-path-to-uri uri "/")))))
      (t
       (make-file-serving-response truename)))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file serving response

(def class* file-serving-response (response)
  ((file-name)))

(def (function e) make-file-serving-response (file-name)
  (make-instance 'file-serving-response :file-name file-name))

(defmethod send-response ((self file-serving-response))
  (server.info "Sending file serving response from ~S" (file-name-of self))
  (serve-file (file-name-of self)
              :headers (headers-of self)
              :cookies (cookies-of self)))

(def method convert-to-primitive-response ((response file-serving-response))
  response)


;;;;;;;;;;;;;;;;;;;
;;; directory index

(def class* directory-index-response (response)
  ((path-prefix)
   (root-directory)
   (relative-path)
   (directory)))

(def (function e) make-directory-index-response (path-prefix relative-path root-directory
                                                 &optional (directory (merge-pathnames relative-path root-directory)))
  (aprog1
      (make-instance 'directory-index-response
                     :path-prefix path-prefix :root-directory root-directory
                     :relative-path relative-path :directory directory)
    (setf (header-value it +header/content-type+) +html-content-type+)))

(defmethod send-response ((self directory-index-response))
  (emit-simple-html-document-http-response (:title (concatenate-string
                                                    "Directory index of \""
                                                    (relative-path-of self)
                                                    "\" under \""
                                                    (path-prefix-of self)
                                                    "\"")
                                            :headers (headers-of self)
                                            :cookies (cookies-of self))
    <table
      ,@(bind ((elements (cl-fad:list-directory (directory-of self)))
               (path-prefix (path-prefix-of self))
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
                    <a (:href ,(concatenate-string path-prefix relative-path name "/"))
                      ,name "/">>>)
          (iter (for file :in files)
                (for name = (apply 'concatenate-string
                                   (pathname-name file)
                                   (awhen (pathname-type file)
                                     (list "." it))))
                <tr
                  <td
                    <a (:href ,(escape-as-uri (concatenate-string path-prefix relative-path name)))
                      ,name>>
                  <td ,(integer-to-string (isys:stat-size (isys:%sys-lstat (namestring file))))>>))>))

;;;;;;;;;;;;;;;;;;;
;;; MIME stuff for serving static files

(def special-variable *mime-type->extensions* nil)
(def special-variable *extension->mime-types* nil)
(def (constant :test 'equal) +mime-types-file+ #P"/etc/mime.types")

(def function ensure-mime-types-are-read ()
  (when (and (null *mime-type->extensions*)
             (probe-file +mime-types-file+))
    (read-mime-types-file +mime-types-file+)))

(def function parse-mime-types-file (mime-types-file visitor)
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

(def function read-mime-types-file (mime-types-file)
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

(def (function e) extensions-for-mime-type (mime-type)
  "Extensions that can be given to file of given MIME type."
  (check-type mime-type string)
  (gethash mime-type *mime-type->extensions*))

(def (function e) mime-types-for-extension (extension)
  "MIME types associated with the given file extension."
  (check-type extension string)
  (gethash extension *extension->mime-types*))

(with-simple-restart (continue "Ignore the error and continue without reading ~A" +mime-types-file+)
  (ensure-mime-types-are-read))
