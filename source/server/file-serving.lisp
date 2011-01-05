;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;; TODO use iolib for file operations

;;;;;;
;;; Directory serving broker cache entry

(def class* directory-serving-broker/cache-entry ()
  ((file-path)
   (file-write-date)
   (bytes-to-respond)
   (content-encoding nil)
   (content-type nil)
   (last-updated-at (get-monotonic-time))
   (last-used-at (get-monotonic-time))))

;;;;;;
;;; Directory serving broker

;; TODO managed in-memory caching of files inside a directory-serving-broker

(def class* directory-serving-broker (broker-at-path-prefix)
  ((root-directory
    :unbound
    :type iolib.pathnames:file-path)
   (allow-access-to-external-files
    #f
    :type boolean
    :accessor allow-access-to-external-files?
    :documentation "Controls whether files outside the subtree of ROOT-DIRECTORY may be served (e.g. reached through a symlink).")
   (render-directory-index
    #t
    :type boolean
    :accessor render-directory-index?
    :documentation "Controls whether to render an HTML response for directories.")
   ;; TODO (files-only #f)
   ;; FIXME: there's a threading issue with file-path->cache-entry. access to the broker state is not serialized! fix and audit all subclasses, too...
   (cache (make-hash-table :test 'equal))
   (path-does-not-exists-response-factory (lambda (&key &allow-other-keys)
                                            (make-not-found-response)))))

(def method shared-initialize :around ((self directory-serving-broker) slot-names &rest args &key root-directory &allow-other-keys)
  (if root-directory
      (bind ((root-directory (iolib.pathnames:file-path root-directory)))
        (unless (iolib.os:directory-exists-p root-directory)
          (warn "Root directory ~A specified for ~A does not exist!" root-directory self))
        (apply #'call-next-method self slot-names :root-directory root-directory
               args))
      (call-next-method)))

(def (function e) make-directory-serving-broker (path-prefix root-directory &key priority)
  (check-type root-directory (or pathname iolib.pathnames:file-path-designator))
  (make-instance 'directory-serving-broker
                 :path-prefix path-prefix
                 :root-directory root-directory
                 :priority priority))

(def method produce-response ((broker directory-serving-broker) (request request))
  (bind ((root-directory (root-directory-of broker))
         (path-prefix (path-prefix-of broker))
         (relative-path (remaining-path-of-request-uri request)))
    (server.debug "PRODUCE-RESPONSE for path-prefix ~S, relative-path ~S, root-directory ~A" path-prefix relative-path root-directory)
    (when (starts-with #\/ relative-path)
      ;; drop any leading /'s
      (setf relative-path (subseq relative-path (position #\/ relative-path :test-not #'char=))))
    (or (make-file-serving-response-for-query-path broker path-prefix relative-path root-directory)
        (funcall (path-does-not-exists-response-factory-of broker)
                 :broker broker
                 :path-prefix path-prefix
                 :relative-path relative-path
                 :root-directory root-directory))))

(def generic make-file-serving-response-for-query-path (broker path-prefix relative-path root-directory)
  (:method ((broker directory-serving-broker) (path-prefix string) (relative-path string) (root-directory iolib.pathnames:file-path))
    (assert (not (starts-with #\/ relative-path)))
    (bind ((absolute-file-path (if (zerop (length relative-path))
                                   root-directory
                                   (ignore-errors (iolib.os:resolve-file-path relative-path :defaults root-directory)))))
      (when (and absolute-file-path
                 (iolib.os:file-exists-p absolute-file-path))
        (make-file-serving-response-for-directory-entry broker absolute-file-path path-prefix relative-path root-directory)))))

(def generic make-directory-serving-broker/cache-key (broker absolute-file-path content-encoding)
  (:method ((broker directory-serving-broker) (absolute-file-path iolib.pathnames:file-path) content-encoding)
    (list (iolib.pathnames:file-path-namestring absolute-file-path) content-encoding)))

(def generic make-file-serving-response-for-directory-entry (broker absolute-file-path path-prefix relative-path root-directory)
  (:method :around ((broker directory-serving-broker) absolute-file-path path-prefix relative-path root-directory)
    ;; intentionally not dispatching on all the arguments, because we don't want this check to be skipped due to a previous misbehavior...
    (when (or (allow-access-to-external-files? broker)
              (starts-with-subseq (iolib.pathnames:file-path-namestring root-directory)
                                  (iolib.pathnames:file-path-namestring absolute-file-path)))
        (call-next-method)))
  (:method ((broker directory-serving-broker) (absolute-file-path iolib.pathnames:file-path)
            (path-prefix string) (relative-path string) (root-directory iolib.pathnames:file-path))
    (files.debug "Making file serving response for ~A, path-prefix ~S, relative-path ~S, root-directory ~S" absolute-file-path path-prefix relative-path root-directory)
    (cond
      ((iolib.os:directory-exists-p absolute-file-path)
       (when (render-directory-index? broker)
         (if (or (ends-with #\/ relative-path)
                 (zerop (length relative-path)))
             (make-directory-index-response path-prefix relative-path root-directory absolute-file-path)
             (make-redirect-response (append-path-to-uri (clone-request-uri) "/")))))
      (t
       (bind ((response-compression (default-response-compression))
              (cache (cache-of broker))
              (cache-key (make-directory-serving-broker/cache-key broker absolute-file-path response-compression))
              (file-write-date/on-disk (file-write-date (iolib.pathnames:file-path-namestring absolute-file-path)))
              ((:values cache-entry cache-entry-found?) (when cache
                                                          (gethash cache-key cache))))
         (if (or (not response-compression)
                 (not (compress-file-before-serving? absolute-file-path)))
             (make-file-serving-response absolute-file-path)
             (progn
               (unless cache-entry
                 (setf cache-entry (make-instance 'directory-serving-broker/cache-entry
                                                  :file-path absolute-file-path
                                                  ;; this will force the recompilation below
                                                  :file-write-date most-negative-fixnum)))
               (bind (((:slots bytes-to-respond last-used-at content-encoding content-type file-write-date) cache-entry))
                 (if (<= file-write-date/on-disk file-write-date)
                     (files.debug "Found valid cached entry for ~A, in ~A" absolute-file-path broker)
                     (progn
                       (setf file-write-date file-write-date/on-disk)
                       (update-directory-serving-broker/cache-entry broker cache-entry absolute-file-path relative-path root-directory response-compression)))
                 (setf last-used-at (get-monotonic-time))
                 (when (and cache
                            (not cache-entry-found?))
                   (setf (gethash cache-key cache) cache-entry))
                 (bind ((response (etypecase bytes-to-respond
                                    ((vector (unsigned-byte 8))
                                     (make-byte-vector-response* bytes-to-respond
                                                                 :last-modified-at (local-time:universal-to-timestamp file-write-date/on-disk)))
                                    (iolib.pathnames:file-path
                                     (make-file-serving-response bytes-to-respond :last-modified-at (local-time:universal-to-timestamp file-write-date/on-disk)))
                                    (response bytes-to-respond))))
                   (when content-encoding
                     (setf (header-value response +header/content-encoding+) content-encoding))
                   (when content-type
                     (setf (header-value response +header/content-type+) content-type))
                   response)))))))))

(def generic update-directory-serving-broker/cache-entry (broker cache-entry absolute-file-path relative-path root-directory response-compression)
  (:method ((broker directory-serving-broker) (cache-entry directory-serving-broker/cache-entry)
            (absolute-file-path iolib.pathnames:file-path) (relative-path string) (root-directory iolib.pathnames:file-path) response-compression)
    (bind (((:slots bytes-to-respond last-used-at content-encoding file-write-date) cache-entry)
           (compressed-file (iolib.pathnames:file-path (shadow-temporary-filename root-directory relative-path "hdws-compressed-static-file-cache"))))
      (files.debug "Updating compressed file cache for ~S, into compressed file ~S" absolute-file-path compressed-file)
      (setf bytes-to-respond compressed-file)
      (ensure-directories-exist (iolib.pathnames:file-path-namestring compressed-file))
      (with-open-file (input (iolib.pathnames:file-path-namestring absolute-file-path) :direction :input :element-type '(unsigned-byte 8))
        (with-open-file (output (iolib.pathnames:file-path-namestring compressed-file) :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
          (bind ((uncompressed-length nil)
                 (compressed-length nil))
            (setf (values uncompressed-length compressed-length content-encoding)
                  (compress-response/stream input output :compression response-compression))
            (finish-output output)
            (bind ((input-length (file-length input))
                   (output-length (file-length output)))
              (files.debug "Updated compressed file cache for ~S, the compressed file is ~S. Old size ~A, new size ~A, ratio ~,3F" absolute-file-path compressed-file input-length output-length (/ output-length input-length))
              (assert (= input-length uncompressed-length))
              (assert output-length)
              (assert (= compressed-length output-length)))))))))

(def special-variable *file-compression/file-extension-blacklist*
  (aprog1
      (make-hash-table :test #'equal)
    (dolist (el '("png" "jpg" "jpeg" "gif"))
      (setf (gethash el it) t))))

(def function compress-file-before-serving? (file)
  (check-type file (or pathname iolib.pathnames:file-path-designator))
  (bind ((extension (iolib.pathnames:file-path-file-type (iolib.pathnames:file-path file))))
    (not (gethash extension *file-compression/file-extension-blacklist*))))

;;;;;;
;;; File serving response

(def class* file-serving-response (response)
  ((file-path
    :unbound
    :type iolib.pathnames:file-path)
   (last-modified-at)
   (delete-file-when-finished
    #f
    :type boolean
    :accessor delete-file-when-finished?)))

(def (function e) make-file-serving-response (file-path &key (delete-when-finished #f) (last-modified-at nil last-modified-at?))
  (bind ((file-path (etypecase file-path
                      (iolib.pathnames:file-path file-path)
                      (string (iolib.pathnames:file-path file-path))))
         (result (make-instance 'file-serving-response :file-path file-path :delete-file-when-finished delete-when-finished)))
    (assert (iolib.pathnames:absolute-file-path-p file-path))
    (when last-modified-at?
      (setf (last-modified-at-of result) last-modified-at))
    result))

(def method send-response ((self file-serving-response))
  (server.info "Sending file serving response from ~S" (file-path-of self))
  ;; TODO convert serve-file to use iolib?
  (apply 'serve-file (iolib.pathnames:file-path-namestring (file-path-of self))
         :headers (headers-of self)
         :cookies (cookies-of self)
         (when (slot-boundp self 'last-modified-at)
           (list :last-modified-at (last-modified-at-of self)))))

(def method close-response :after ((self file-serving-response))
  (when (delete-file-when-finished? self)
    (iolib.os:delete-files (file-path-of self))))

(def method convert-to-primitive-response ((response file-serving-response))
  response)

;;;;;;
;;; Directory index

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
    (setf (header-value it +header/content-type+) (content-type-for +html-mime-type+ (encoding-name-of it)))))

(def method convert-to-primitive-response ((self directory-index-response))
  (bind ((title (string+ "Directory index of \"" (relative-path-of self) "\" under \"" (path-prefix-of self) "\""))
         (body (with-output-to-sequence (*xml-stream* :external-format (external-format-of self)
                                                      :initial-buffer-size 256)
                 (emit-html-document (:content-type +html-content-type+ :title title)
                   (render-directory-as-html (directory-of self) (path-prefix-of self) (relative-path-of self))))))
    (make-byte-vector-response* body
                                :headers (headers-of self)
                                :cookies (cookies-of self))))

(def function render-directory-as-html/default-filter (&key name kind &allow-other-keys)
  (declare (ignore kind))
  ;; by default exclude .foo, both files and dirs
  (not (starts-with #\. name)))

(def function render-directory-as-html (directory path-prefix relative-path &key (filter 'render-directory-as-html/default-filter))
  (labels ((render-url (path name)
             <a (:href ,(escape-as-uri path) :class ,path)
                ,name>)
           (render-file (name path)
             <tr <td ,(render-url name name)>
                 ;; TODO use something higher level than isys:lstat
                 <td ,(integer-to-string (isys:stat-size (isys:stat (iolib.pathnames:file-path-namestring path))))>>)
           (render-directory (name)
             <tr <td ,(render-url name (string+ name "/"))>
                 <td>>)
           (render-entry (name kind relative-parent depth)
             (declare (ignore depth))
             (bind ((parent (iolib.pathnames:merge-file-paths relative-parent directory))
                    (path (iolib.pathnames:merge-file-paths name parent)))
               (ecase kind
                 (:directory (render-directory name))
                 (:regular-file (render-file name path))
                 ;; we ignore symlinks, because they should be resolved already at this point. if not, then it's a broken one.
                 (:symbolic-link)))))
     <table
       <tr <td ,(render-url (string+ path-prefix relative-path "../") "[parent directory]")>
           <td>>
       ,(progn
          (iolib.os:walk-directory directory #'render-entry
                                   :maxdepth 1
                                   :test (lambda (name kind)
                                           (funcall filter :name name :kind kind))
                                   :key 'iolib.pathnames:file-path-namestring
                                   :follow-symlinks #t)
          nil)>))

;;;;;;
;;; MIME stuff for serving static files

(def special-variable *mime-type->extensions* (make-hash-table :test #'equal))
(def special-variable *extension->mime-types* (make-hash-table :test #'equal))
(def constant +mime-types-file+ #P"/etc/mime.types")

(def function ensure-mime-types-are-read ()
  (when (and (zerop (hash-table-count *mime-type->extensions*))
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
                             (cl-ppcre:split #.(format nil "( |~A)" #\Tab) line))) ; make that tab char survive tab killing zealots
    (unless (null pieces)
      (funcall visitor pieces))))

(def function read-mime-types-file (mime-types-file)
  "Read in /etc/mime.types file."
  (clrhash *mime-type->extensions*)
  (clrhash *extension->mime-types*)
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
