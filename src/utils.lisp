;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (generic e) debug-on-error (context error)
  (:method (context error)
    *debug-on-error*))

(deftype simple-ub8-vector (&optional (length '*))
  `(simple-array (unsigned-byte 8) (,length)))

(def (function io) handle-otherwise (otherwise)
  (cond
    ((eq otherwise :error)
     (error "Otherwise assertion failed"))
    ((and (consp otherwise)
          (member (first otherwise) '(:error :warn)))
     (case (first otherwise)
       (:error (apply #'error (second otherwise) (nthcdr 2 otherwise)))
       (:warn (apply #'warn (second otherwise) (nthcdr 2 otherwise)))))
    ((functionp otherwise)
     (funcall otherwise))
    (t
     otherwise)))

(def special-variable *temporary-file-random* (princ-to-string (nix:getpid)))
(def special-variable *temporary-file-unique-number* 0)

(defun filename-for-temporary-file (&optional (prefix "wui-"))
  (concatenate 'string
               *directory-for-temporary-files*
               prefix
               *temporary-file-random*
               "-"
               ;; TODO atomic-incf
               (princ-to-string (incf *temporary-file-unique-number*))))

(defun open-temporary-file (&rest args &key
                            (element-type '(unsigned-byte 8))
                            (direction :output)
                            (name-prefix "wui-"))
  (remove-from-plistf args :name-prefix)
  (iter
    (for file-name = (filename-for-temporary-file name-prefix))
    (for file = (apply #'open
                       file-name
                       :if-exists nil
                       :direction direction
                       :element-type element-type
                       args))
    (until file)
    (finally (return (values file file-name)))))

(declaim (ftype (function () double-float) get-monotonic-time))
(def (function eio) get-monotonic-time ()
  "Returns a time in seconds as a double-float that constantly grows (unaffected by setting the system clock)."
  (declare (inline nix:clock-gettime))
  (bind (((:values seconds nano-seconds) (nix:clock-gettime nix:clock-monotonic)))
    (+ seconds (/ nano-seconds 1000000000d0))))

(def (function i) us-ascii-octets-to-string (vector)
  (coerce (babel:octets-to-string vector :encoding :us-ascii) 'simple-base-string))
(def (function i) string-to-us-ascii-octets (string)
  (babel:string-to-octets string :encoding :us-ascii))

(def (function i) iso-8859-1-octets-to-string (vector)
  (babel:octets-to-string vector :encoding :iso-8859-1))
(def (function i) string-to-iso-8859-1-octets (string)
  (babel:string-to-octets string :encoding :iso-8859-1))

(def (function i) utf-8-octets-to-string (vector)
  (babel:octets-to-string vector :encoding :utf-8))
(def (function i) string-to-utf-8-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defgeneric encoding-name-of (thing)
  (:method ((self external-format))
    (encoding-name-of (external-format-encoding self)))
  (:method ((self babel-encodings:character-encoding))
    (babel-encodings:enc-name self)))

#+sbcl
(defmacro with-thread-name (name &body body)
  (with-unique-names (thread previous-name)
    `(let* ((,thread sb-thread:*current-thread*)
            (,previous-name (sb-thread:thread-name ,thread)))
       (setf (sb-thread:thread-name ,thread)
             (concatenate-string ,previous-name ,name))
       (unwind-protect
            (progn
              ,@body)
         (setf (sb-thread:thread-name ,thread) ,previous-name)))))

#-sbcl
(defmacro with-thread-name (name &body body)
  (declare (ignore name))
  `(progn
     ,@body))

(def (function io) shrink-vector (vector size)
  "Fast shrinking for simple vectors. It's not thread-safe, use only on local vectors!"
  #+allegro
  (excl::.primcall 'sys::shrink-svector vector size)
  #+sbcl
  (setq vector (sb-kernel:%shrink-vector vector size))
  #+cmu
  (lisp::shrink-vector vector size)
  #+lispworks
  (system::shrink-vector$vector vector size)
  #+scl
  (common-lisp::shrink-vector vector size)
  #-(or allegro cmu lispworks sbcl scl)
  (setq vector (subseq vector 0 size))
  vector)

(def (function io) make-adjustable-vector (initial-length &key (element-type t))
  (declare (type array-index initial-length))
  (make-array initial-length :adjustable #t :fill-pointer 0 :element-type element-type))

(def (function i) make-displaced-array (array &optional (start 0) (end (length array)))
  (make-array (- end start)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset start))

(def (function o) split-ub8-vector (separator line)
  (declare (type simple-ub8-vector line)
           (inline make-displaced-array))
  (iter outer ; we only need the outer to be able to collect a last chunk in the finally block of the inner loop
        (iter (with start = 0)
              (for end :upfrom 0)
              (for char :in-vector line)
              (declare (type fixnum start end))
              (when (= separator char)
                (in outer (collect (make-displaced-array line start end)))
                (setf start (1+ end)))
              (finally (in outer (collect (make-displaced-array line start)))))
        (while nil)))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(def (function i) is-lock-held? (lock)
  #+sbcl (eq (sb-thread::mutex-value lock) (current-thread))
  #-sbcl #t)

;;;;;;;;;;;;;;;;
;;; string utils

(def (function o) concatenate-string (&rest args)
  (declare (dynamic-extent args))
  (apply #'concatenate 'string args))

(def compiler-macro concatenate-string (&rest args)
  `(concatenate 'string ,@args))

(def (constant :test 'string=) +lower-case-ascii-alphabet+ (coerce "abcdefghijklmnopqrstuvwxyz" 'simple-base-string))
(def (constant :test 'string=) +upper-case-ascii-alphabet+ (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'simple-base-string))
(def (constant :test 'string=) +ascii-alphabet+ (coerce (concatenate 'string +upper-case-ascii-alphabet+ +lower-case-ascii-alphabet+) 'simple-base-string))
(def (constant :test 'string=) +alphanumeric-ascii-alphabet+ (coerce (concatenate 'string +ascii-alphabet+ "0123456789") 'simple-base-string))
(def (constant :test 'string=) +base64-alphabet+ (coerce (concatenate 'string +alphanumeric-ascii-alphabet+ "+/") 'simple-base-string))

(def (function o) random-string (&optional (length 32) (alphabet +ascii-alphabet+))
  (etypecase alphabet
    (simple-base-string
     (random-simple-base-string length alphabet))
    (string
     (loop
        :with result = (make-string length)
        :with alphabet-length = (length alphabet)
        :for i :below length
        :do (setf (aref result i) (aref alphabet (random alphabet-length)))
        :finally (return result)))))

(def (function o) random-simple-base-string (&optional (length 32) (alphabet +ascii-alphabet+))
  (declare (type array-index length)
           (type simple-base-string alphabet))
  (loop
     :with result = (make-string length :element-type 'base-char)
     :with alphabet-length = (length alphabet)
     :for i :below length
     :do (setf (aref result i) (aref alphabet (random alphabet-length)))
     :finally (return result)))

(def (function o) new-random-hash-table-key (hash-table key-length)
  (iter (for key = (random-simple-base-string key-length))
        (for (values value foundp) = (gethash key hash-table))
        (when (not foundp)
          (return key))))

(def (function io) insert-with-new-random-hash-table-key (hash-table key-length value)
  (bind ((key (new-random-hash-table-key hash-table key-length)))
    (setf (gethash key hash-table) value)
    (values key value)))


;;;;;;;;;;;;;;;;;;;;
;;; xhtml generation

(def (macro e) with-html-stream (stream &body body)
  `(bind ((*html-stream* ,stream))
     ,@body))

(def (macro e) emit-into-html-stream (stream &body body)
  `(bind ((*html-stream* ,stream))
     (emit *quasi-quoted-xml-transformation*
           (progn
             ,@body))))

(def (macro e) emit-http-response ((&optional headers-as-plist cookie-list) &body body)
  "Emit a full http response and also bind html stream, so you are ready to output directly into the network stream."
  `(emit-into-html-stream (network-stream-of *request*)
     (send-http-headers (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                      (collect `(cons ,name ,value))))
                        (list ,@cookie-list))
     ,@body))

(def (macro e) emit-http-response* ((&optional headers cookies) &body body)
  "Just like EMIT-HTML-RESPONSE, but HEADERS and COOKIES are simply evaluated as expressions."
  `(emit-into-html-stream (network-stream-of *request*)
     (send-http-headers ,headers ,cookies)
     ,@body))

(defmacro within-xhtml-tag (tag-name &body body)
  "Execute BODY and wrap its html output in a TAG-NAME xml node with \"http://www.w3.org/1999/xhtml\" xml namespace."
  `<,,tag-name ("xmlns" #.+XHTML-NAMESPACE-URI+)
     ,,@body>)
