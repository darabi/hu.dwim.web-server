;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* js-directory-serving-broker (directory-serving-broker)
  ()
  (:metaclass funcallable-standard-class))

(def (function e) make-js-directory-serving-broker (path-prefix root-directory &key priority)
  (make-instance 'js-directory-serving-broker :path-prefix path-prefix :root-directory root-directory :priority priority))

(def method make-file-serving-response-for-query-path ((broker js-directory-serving-broker) path-prefix relative-path root-directory)
  (when (and (or (zerop (length relative-path))
                 (alphanumericp (elt relative-path 0)))
             (ends-with-subseq ".js" relative-path))
    (bind ((pathname (merge-pathnames (make-pathname :type "lisp")
                                      (merge-pathnames relative-path
                                                       root-directory)))
           (truename (ignore-errors
                       (probe-file pathname))))
      (files.dribble "Looking for file ~A, truename ~A, in ~A" pathname truename broker)
      (when truename
        (make-file-serving-response-for-directory-entry broker truename path-prefix relative-path root-directory)))))

(def function js-directory-serving-broker/make-cache-key (truename &optional content-encoding)
  (if content-encoding
      (list (namestring truename) content-encoding)
      (namestring truename)))

(def method make-file-serving-response-for-directory-entry ((broker js-directory-serving-broker) truename path-prefix relative-path root-directory)
  (bind ((compress? (and (not *disable-response-compression*)
                         (accepts-encoding? +content-encoding/deflate+)))
         (key (js-directory-serving-broker/make-cache-key truename (when compress? :deflate)))
         (cache (file-path->cache-entry-of broker))
         (cache-entry (gethash key cache))
         (file-write-date (file-write-date truename))
         (bytes-to-serve nil))
    (if (and cache-entry
             (<= file-write-date (file-write-date-of cache-entry)))
        (progn
          (files.dribble "Found cached entry for ~A, in ~A" truename broker)
          (setf (last-used-at-of cache-entry) (get-monotonic-time))
          (setf bytes-to-serve (bytes-to-respond-of cache-entry)))
        (unless (cl-fad:directory-pathname-p truename)
          (files.debug "Compiling and updating cache entry for ~A, in ~A" truename broker)
          (setf bytes-to-serve (compile-js-file-to-byte-vector broker truename :encoding :utf-8))
          (when compress?
            (bind ((compressed-bytes (hu.dwim.wui.zlib:allocate-compress-buffer bytes-to-serve))
                   (compressed-bytes-length (hu.dwim.wui.zlib:compress bytes-to-serve compressed-bytes)))
              (files.debug "Compressed response for cache entry ~A, original-size ~A, compressed-size ~A, ratio: ~,3F" truename (length bytes-to-serve) compressed-bytes-length (/ compressed-bytes-length (length bytes-to-serve)))
              (setf bytes-to-serve (coerce-to-simple-ub8-vector compressed-bytes compressed-bytes-length))
              (assert (= (length bytes-to-serve) compressed-bytes-length))))
          (unless cache-entry
            (setf cache-entry (make-directory-serving-broker/cache-entry truename))
            (setf (gethash key cache) cache-entry))
          (setf (file-write-date-of cache-entry) file-write-date)
          (setf (bytes-to-respond-of cache-entry) bytes-to-serve)))
    (aprog1
        (make-byte-vector-response* bytes-to-serve
                                    :last-modified-at (local-time:universal-to-timestamp file-write-date)
                                    :seconds-until-expires (* 60 60)
                                    :content-type (content-type-for +javascript-mime-type+ :utf-8))
      (when compress?
        (setf (header-value it +header/content-encoding+) +content-encoding/deflate+)))))

(def generic compile-js-file-to-byte-vector (broker filename &key encoding)
  (:method ((broker js-directory-serving-broker) filename &key (encoding +encoding+))
    (bind ((body-as-string (read-file-into-string filename :external-format encoding)))
      (setf body-as-string (concatenate-string "`js(progn "
                                               body-as-string
                                               ")"))
      (bind ((*package* (find-package :hu.dwim.wui)))
        (with-local-readtable
          (setup-readtable)
          (enable-js-sharpquote-syntax)
          (coerce-to-simple-ub8-vector (with-output-to-sequence (*js-stream*)
                                         (bind ((forms (read-from-string body-as-string)))
                                           (eval forms)))))))))

;; FIXME: this generates a quite big list which is redundant like hell
;;        but it is cached and compressed, so we don't care about it right now
(def function serve-js-component-class-hierarchy ()
  (bind ((component-class (find-class 'component)))
    `js (setf wui.component-class-precedence-lists (create))
    (dolist (subclass (subclasses component-class))
      (bind ((subclass-name (class-name-as-string subclass))
             (class-precedence-list (class-precedence-list subclass))
             (index (position component-class class-precedence-list))
             (class-name-precedence-list (mapcar #'class-name-as-string (subseq class-precedence-list 0 (1+ index)))))
        `js(setf (slot-value wui.component-class-precedence-lists ,subclass-name) (array ,@class-name-precedence-list))))))
