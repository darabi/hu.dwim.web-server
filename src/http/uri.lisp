;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;; http://www.faqs.org/rfcs/rfc2396.html

(export '(uri-of host-of scheme-of path-of fragment-of))

(define-condition uri-parse-error (simple-parse-error)
  ())

(defun uri-parse-error (message &rest args)
  (error 'uri-parse-error
         :format-control message
         :format-arguments args))

(def class* uri ()
  ((scheme    nil)
   (host      nil)
   (port      nil)
   (path      nil)
   (query     nil)
   (query-parameters)
   (fragment  nil)))

(def method make-load-form ((self uri) &optional env)
  (make-load-form-saving-slots self :environment env))

(def print-object (uri :identity nil)
  (write-uri -self- *standard-output* nil))

(def (function ie) make-uri (&key scheme host port path query fragment)
  (make-instance 'uri :scheme scheme :host host :port port :path path
                 :query query :fragment fragment))

(def (function ie) clone-uri (uri)
  (bind ((result (make-instance 'uri
                                :scheme (scheme-of uri) :host (host-of uri) :port (port-of uri) :path (path-of uri)
                                :query (query-of uri) :fragment (fragment-of uri))))
    (when (slot-boundp uri 'query-parameters)
      (setf (query-parameters-of result) (copy-alist (query-parameters-of uri))))
    result))

(defmethod query-parameters-of :before ((self uri))
  (unless (slot-boundp self 'query-parameters)
    (setf (query-parameters-of self) (awhen (query-of self)
                                       (parse-query-parameters it)))))

(defun uri-query-parameter-value (uri name)
  (cdr (assoc name (query-parameters-of uri) :test #'string=)))

(defun (setf uri-query-parameter-value) (value uri name)
  (bind ((entry (assoc name (query-parameters-of uri) :test #'string=)))
    (if entry
        (setf (cdr entry) value)
        (add-query-parameter-to-uri uri name value))))

(defun add-query-parameter-to-uri (uri name value)
  (nconcf (query-parameters-of uri) (list (cons name value))))

(defun clear-uri-query-parameters (uri)
  (setf (query-parameters-of uri) '()))

(defun append-path-to-uri (uri path)
  (setf (path-of uri) (concatenate 'string (path-of uri) path))
  uri)

(def (function o) write-uri-sans-query (uri stream &optional (escape t))
  "Write URI to STREAM, only write scheme, host and path."
  (bind ((scheme (scheme-of uri))
         (host (host-of uri))
         (port (port-of uri))
         (path (path-of uri)))
    (flet ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)))
      (when scheme
        (out scheme)
        (write-string "://" stream))
      (when host
        ;; don't escape host
        (etypecase host
          (ipv6-address
           (write-char #\[ stream)
           (write-string (address-to-string host) stream)
           (write-char #\] stream))
          (ipv4-address
           (write-string (address-to-string host) stream))
          (string
           (write-string host stream))))
      (when port
        (write-string ":" stream)
        (princ port stream))
      (when path
        (out path)))))

(def (function o) write-query-parameters (parameters stream &optional (escape t))
  (labels ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)
             (values))
           (write-query-part (name value)
             (if (consp value)
                 (dolist (el value)
                   (out name)
                   (write-char #\= stream)
                   (write-query-value el))
                 (progn
                   (out name)
                   (write-char #\= stream)
                   (write-query-value value))))
           (write-query-value (value)
             (out (typecase value
                    (number (princ-to-string value))
                    (null "")
                    (t (string value))))))
    (iter (for (name . value) :in parameters)
          (write-char (if (first-iteration-p) #\? #\&) stream)
          (write-query-part name value))))

(def (function o) write-uri (uri stream &optional (escape t))
  (write-uri-sans-query uri stream escape)
  (labels ((out (string)
             (funcall (if escape
                          #'write-as-uri
                          #'write-string)
                      string stream)))
    (write-query-parameters (query-parameters-of uri) stream escape)
    (awhen (fragment-of uri)
      (write-char #\# stream)
      (out it))))

(defun print-uri-to-string (uri &optional (escape t))
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (string)
      (write-uri uri string escape))))

(defun print-uri-to-string-sans-query (uri)
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (string)
      (write-uri-sans-query uri string))))

(def (constant :test 'equalp) +uri-escaping-ok-table+
  (bind ((result (make-array 256
                             :element-type 'boolean
                             :initial-element nil)))
    ;; The list of characters which don't need to be escaped when writing URIs.
    ;; This list is inherently a heuristic, because different uri components may have
    ;; different escaping needs, but it should work fine for http.
    (loop
       :for ok-char across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_~/"
       :do (setf (aref result (char-code ok-char)) t))
    (coerce result '(simple-array boolean (256)))))

(defun escape-as-uri (string)
  "Escapes all non alphanumeric characters in STRING following the URI convention. Returns a fresh string."
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (escaped nil :element-type 'base-char)
      (write-as-uri string escaped))))

(def (function o) write-as-uri (string stream)
  (declare (type vector string)
           (type stream stream))
  (loop
     :for char-code :of-type (unsigned-byte 8) :across (the (vector (unsigned-byte 8))
                                                         (string-to-utf-8-octets string))
     :do (if (aref #.+uri-escaping-ok-table+ char-code)
             (write-char (code-char char-code) stream)
             (format stream "%~2,'0X" char-code))))

(def (function io) unescape-as-uri (string)
  (%unescape-as-uri string))

(def (function o) %unescape-as-uri (input)
  "URI unescape based on http://www.ietf.org/rfc/rfc2396.txt"
  (declare (type string input))
  (let ((input-length (length input)))
    (when (zerop input-length)
      (return-from %unescape-as-uri ""))
    (bind ((input-index 0)
           (output (make-array input-length :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (declare (type fixnum input-length input-index))
      (labels ((read-next-char (must-exists-p)
                 (when (>= input-index input-length)
                   (if must-exists-p
                       (uri-parse-error "Unexpected end of input")
                       (return-from %unescape-as-uri (utf-8-octets-to-string output))))
                 (prog1
                     (aref input input-index)
                   (incf input-index)))
               (write-next-byte (byte)
                 (declare (type (unsigned-byte 8) byte))
                 (vector-push-extend byte output)
                 (values))
               (char-to-int (char)
                 (let ((result (digit-char-p char 16)))
                   (unless result
                     (uri-parse-error "Expecting a digit and found ~S" char))
                   result))
               (parse ()
                 (let ((next-char (read-next-char nil)))
                   (case next-char
                     (#\% (char%))
                     (#\+ (char+))
                     (t (write-next-byte (char-code next-char))))
                   (parse)))
               (char% ()
                 (write-next-byte (+ (ash (char-to-int (read-next-char t)) 4)
                                     (char-to-int (read-next-char t))))
                 (values))
               (char+ ()
                 (write-next-byte +space+)))
        (parse)))))

(def (function io) parse-uri (uri)
  (%parse-uri (coerce uri 'simple-base-string)))

(def (function o) %parse-uri (uri)
  (declare (type simple-base-string uri))
  ;; can't use :sharedp, because we expect the returned strings to be simple-base-string's
  (bind ((pieces (nth-value 1 (cl-ppcre:scan-to-strings "^(([^:/?#]+):)?(//([^:/?#]*)(:([0-9]+))?)?([^?#]*)(\\?([^#]*))?(#(.*))?"
                                                        uri :sharedp #f))))
    (make-uri :scheme   (aref pieces 1)
              :host     (aref pieces 3)
              :port     (aref pieces 5)
              :path     (aref pieces 6)
              :query    (aref pieces 8)
              :fragment (aref pieces 10))))

(def macro record-query-parameter (param params)
  (declare (type cons param))
  (once-only (param)
    `(bind ((entry (assoc (car ,param) ,params :test #'string=)))
       (if entry
           (progn
             (unless (consp (cdr entry))
               (setf (cdr entry) (list (cdr entry))))
             (nconcf (cdr entry) (list (cdr ,param))))
           (push ,param ,params))
       ,params)))

(def (function o) parse-query-parameters (param-string &optional (initial-parameters (list)))
  "Parse PARAM-STRING into an alist. Contains a list as value when the given parameter was found more than once."
  (declare (type simple-base-string param-string))
  (flet ((grab-param (start separator-position end)
           (declare (type array-index start end)
                    (type (or null array-index) separator-position))
           (bind ((key-start start)
                  (key-end (or separator-position end))
                  (key (make-displaced-array param-string key-start key-end))
                  (value-start (if separator-position
                                   (1+ separator-position)
                                   end))
                  (value-end end)
                  (value (if (zerop (- value-end value-start))
                             ""
                             (make-displaced-array param-string value-start value-end)))
                  (unescaped-key (unescape-as-uri key))
                  (unescaped-value (unescape-as-uri value)))
             (http.dribble "Grabbed parameter ~S with value ~S" unescaped-key unescaped-value)
             (cons unescaped-key unescaped-value))))
    (when (and param-string
               (< 0 (length param-string)))
      (iter
        (with start = 0)
        (with separator-position = nil)
        (with result = initial-parameters)
        (for char :in-vector param-string)
        (for index :upfrom 0)
        (switch (char :test #'char=)
          (#\& ;; end of the current param
           (setf result (record-query-parameter (grab-param start separator-position index) result))
           (setf start (1+ index))
           (setf separator-position nil))
          (#\= ;; end of name
           (setf separator-position index)))
        (finally
         (return (nreverse (record-query-parameter (grab-param start separator-position (1+ index)) result))))))))
