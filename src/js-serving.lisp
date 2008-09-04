;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* js-file-serving-broker (file-serving-broker)
  ()
  (:metaclass funcallable-standard-class))

(def (function e) make-js-file-serving-broker (path-prefix root-directory)
  (make-instance 'js-file-serving-broker :path-prefix path-prefix :root-directory root-directory))

(def method make-file-serving-response-for-query-path ((broker js-file-serving-broker) path-prefix relative-path root-directory)
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

(defun js-file-serving-broker/make-cache-key (truename)
  (namestring truename))

(def (constant :test 'equal) +js-file-serving-broker-response-headers+ `((,+header/content-type+ . ,+javascript-content-type+)))

(def method make-file-serving-response-for-directory-entry ((broker js-file-serving-broker) truename path-prefix relative-path root-directory)
  (bind ((key (js-file-serving-broker/make-cache-key truename))
         (cache (file-path->cache-entry-of broker))
         (cache-entry (gethash key cache))
         (file-write-date (file-write-date truename)))
    ;; TODO add expires, handle if-modified-since... by using a functional-response and serve-sequence?
    (if (and cache-entry
             (<= file-write-date (file-write-date-of cache-entry)))
        (progn
          (files.dribble "Found cached entry for ~A, in ~A" truename broker)
          (setf (last-used-at-of cache-entry) (get-monotonic-time))
          (make-byte-vector-response* (bytes-to-respond-of cache-entry)
                                      :headers +js-file-serving-broker-response-headers+))
        (unless (cl-fad:directory-pathname-p truename)
          (files.debug "Compiling and updating cache entry for ~A, in ~A" truename broker)
          (bind ((bytes (compile-js-file-to-byte-vector broker truename)))
            (unless cache-entry
              (setf cache-entry (make-file-serving-broker/cache-entry truename))
              (setf (gethash key cache) cache-entry))
            (setf (file-write-date-of cache-entry) file-write-date)
            (setf (bytes-to-respond-of cache-entry) bytes)
            (make-byte-vector-response* bytes
                                        :headers +js-file-serving-broker-response-headers+))))))

(def generic compile-js-file-to-byte-vector (broker filename)
  (:method ((broker js-file-serving-broker) filename)
    (bind ((body-as-string (read-file-into-string filename :external-format +encoding+)))
      (setf body-as-string (concatenate-string "`js(progn "
                                               body-as-string
                                               ")"))
      (bind ((*package* (find-package :hu.dwim.wui)))
        (with-local-readtable
          (setup-readtable)
          (enable-js-sharpquote-syntax)
          (with-output-to-sequence (*js-stream*)
            (bind ((forms (read-from-string body-as-string)))
              (eval forms))))))))
