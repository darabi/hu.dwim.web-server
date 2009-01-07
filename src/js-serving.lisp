;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* js-file-serving-broker (file-serving-broker)
  ()
  (:metaclass funcallable-standard-class))

(def (function e) make-js-file-serving-broker (path-prefix root-directory &key priority)
  (make-instance 'js-file-serving-broker :path-prefix path-prefix :root-directory root-directory :priority priority))

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

(def method make-file-serving-response-for-directory-entry ((broker js-file-serving-broker) truename path-prefix relative-path root-directory)
  (bind ((key (js-file-serving-broker/make-cache-key truename))
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
          (unless cache-entry
            (setf cache-entry (make-file-serving-broker/cache-entry truename))
            (setf (gethash key cache) cache-entry))
          (setf (file-write-date-of cache-entry) file-write-date)
          (setf (bytes-to-respond-of cache-entry) bytes-to-serve)))
    ;; TODO this will probably look much different in the lights of multiplexing, but works ok for now
    (make-functional-response*
     (lambda ()
       (serve-js bytes-to-serve
                 :last-modified-at (local-time:universal-to-timestamp file-write-date)
                 :seconds-until-expires (if (and (boundp '*application*)
                                                 (running-in-test-mode-p *application*))
                                            30
                                            (* 60 60))))
     :raw #t)))

(def content-serving-function serve-js (sequence &key
                                                 (last-modified-at (local-time:now))
                                                 headers
                                                 cookies
                                                 content-disposition
                                                 (stream (client-stream-of *request*))
                                                 (encoding (encoding-name-of (external-format-of stream)))
                                                 (content-type (content-type-for +javascript-mime-type+ encoding))
                                                 (seconds-until-expires #.(* 60 60)))
    (:last-modified-at last-modified-at
     :seconds-until-expires seconds-until-expires
     :headers headers
     :cookies cookies
     :stream stream)
  (bind ((bytes (if (stringp sequence)
                    (string-to-octets sequence :encoding (external-format-of stream))
                    sequence)))
    (setf (header-alist-value headers +header/content-type+) content-type)
    (setf (header-alist-value headers +header/content-length+) (integer-to-string (length bytes)))
    (awhen content-disposition
      (setf (header-alist-value headers +header/content-disposition+) it))
    (send-http-headers headers cookies :stream stream)
    (write-sequence bytes stream)))

(def generic compile-js-file-to-byte-vector (broker filename &key encoding)
  (:method ((broker js-file-serving-broker) filename &key (encoding +encoding+))
    (bind ((body-as-string (read-file-into-string filename :external-format encoding)))
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
