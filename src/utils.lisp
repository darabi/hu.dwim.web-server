;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (macro e) delay (&body forms)
  `(lambda ()
     ,@forms))

(def (function e) force (value)
  (if (functionp value)
      (funcall value)
      value))

(def macro notf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect `(not ,place)))))

(def (function io) find-slot (class-or-name slot-name)
  (find slot-name
        (the list
          (class-slots (if (symbolp class-or-name)
                           (find-class class-or-name)
                           class-or-name)))
        :key 'slot-definition-name
        :test 'eq))

(def function mandatory-argument ()
  (error "A mandatory argument was not specified"))

(def (function i) class-prototype (class)
  (closer-mop:class-prototype (ensure-finalized class)))

(def (function i) class-slots (class)
  (closer-mop:class-slots (ensure-finalized class)))

(def (function i) class-precedence-list (class)
  (closer-mop:class-precedence-list (ensure-finalized class)))

(def function qualified-symbol-name (symbol)
  (concatenate 'string (package-name (symbol-package symbol)) "::" (symbol-name symbol)))

(def function trim-suffix (suffix sequence)
  (subseq sequence 0 (- (length sequence) (length suffix))))

(def function every-type-p (type list)
  (every [typep !1 type] list))

(def function optional-list (&rest elements)
  (remove nil elements))

(def function the-only-element (elements)
  (assert (= 1 (length elements)))
  (first elements))

(def function filter (element list &key (key #'identity) (test #'eq))
  (remove element list :key key :test-not test))

(def function filter-if (predicate list &key (key #'identity))
  (remove-if (complement predicate) list :key key))

(def function filter-slots (names slots)
  (filter-if (lambda (slot)
               (member (slot-definition-name slot) names))
             slots))

(def function partition (list &rest predicates)
  (iter (with result = (make-array (length predicates) :initial-element nil))
        (for element :in list)
        (iter (for predicate :in predicates)
              (for index :from 0)
              (when (funcall predicate element)
                (push element (aref result index))
                (finish)))
        (finally
         (return
           (iter (for element :in-vector result)
                 (collect (nreverse element)))))))

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

(def generic hash-key-for (instance)
  (:method ((instance standard-object))
    instance))

;;;;;;
;;; Tree

(def (function o) find-ancestor (node parent-function map-function)
  (ensure-functionf parent-function map-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (while current-node)
        (when (funcall map-function current-node)
          (return current-node))))

(def (function o) find-root (node parent-function)
  (ensure-functionf parent-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (for previous-node :previous current-node)
        (while current-node)
        (finally (return previous-node))))

(def (function o) map-parent-chain (node parent-function map-function)
  (ensure-functionf parent-function map-function)
  (iter (for current-node :initially node :then (funcall parent-function current-node))
        (while current-node)
        (funcall map-function current-node)))

(def (function o) map-tree (node children-function map-function)
  (ensure-functionf children-function map-function)
  (map-tree* node children-function
             (lambda (node parent level)
               (declare (ignore parent level))
               (funcall map-function node))))

(def (function o) map-tree* (node children-function map-function &optional (level 0) parent)
  (declare (type fixnum level))
  (ensure-functionf children-function map-function)
  (cons (funcall map-function node parent level)
        (map 'list (lambda (child)
                     (map-tree* child children-function map-function (1+ level) node))
             (funcall children-function node))))

;;;;;;
;;; Temporary file

(def special-variable *temporary-file-random* (princ-to-string (nix:getpid)))
(def special-variable *temporary-file-unique-number* 0)

(defun filename-for-temporary-file (&optional (prefix "wui-"))
  (concatenate 'string
               *directory-for-temporary-files*
               prefix
               *temporary-file-random*
               "-"
               ;; TODO atomic-incf
               (integer-to-string (incf *temporary-file-unique-number*))))

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

(defmacro with-thread-name (name &body body)
  (declare (ignorable name))
  #*((:sbcl
      (with-unique-names (thread previous-name)
        `(let* ((,thread sb-thread:*current-thread*)
                (,previous-name (sb-thread:thread-name ,thread)))
           (setf (sb-thread:thread-name ,thread)
                 (concatenate-string ,previous-name ,name))
           (unwind-protect
                (progn
                  ,@body)
             (setf (sb-thread:thread-name ,thread) ,previous-name)))))
     (t
      `(progn
         ,@body))))

(def function get-bytes-allocated ()
  "Returns a monotonic counter of bytes allocated, preferable a per-thread value."
  #*((:sbcl
      ;; as of 1.0.22 this is still a global value shared by all threads
      (sb-ext:get-bytes-consed))
     (t
      0)))

(def function call-with-profiling (thunk)
  (block nil
    #+#.(hu.dwim.wui::sbcl-with-symbol '#:sb-sprof '#:start-profiling)
    (return
      (progn
        (sb-sprof:start-profiling)
        (multiple-value-prog1
            (unwind-protect
                 (funcall thunk)
              #+nil ;; TODO: KLUDGE: this makes profiling useless, don't know why
              (sb-sprof:stop-profiling)))))
    (warn "No profiling is available for your lisp, see HU.DWIM.WUI::CALL-WITH-PROFILING for details.")
    (funcall thunk)))

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

(def function owner-class-of-effective-slot-definition (effective-slot)
  "Returns the class to which the given slot belongs."
  #+sbcl(slot-value effective-slot 'sb-pcl::%class)
  #-sbcl(not-yet-implemented))

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

(def (constant e :test 'string=) +missing-resource-css-class+ (coerce "missing-resource" 'simple-base-string))

(def function localized-string-reader (stream c1 c2)
  (declare (ignore c2))
  (unread-char c1 stream)
  (let ((key (read stream)))
    (if (ends-with-subseq "<>" key)
        `(bind (((:values str foundp) (lookup-resource ,(string-downcase (subseq key 0 (- (length key) 2))) :otherwise nil)))
           ,(when (and (> (length key) 0)
                       (upper-case-p (elt key 0)))
              `(setf str (capitalize-first-letter str)))
           (if foundp
               `xml ,str
               <span (:class #.+missing-resource-css-class+)
                 ,str>))
        `(bind (((:values str foundp) (lookup-resource ,(string-downcase key) :otherwise nil)))
           (declare (ignorable foundp))
           ,(when (and (> (length key) 0)
                       (upper-case-p (elt key 0)))
              `(when foundp
                 (setf str (capitalize-first-letter str))))
           str))))

(def function mailto-href (email-address)
  (concatenate 'string "mailto:" email-address))

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

(def (function io) random-simple-base-string (&optional (length 32) (alphabet +ascii-alphabet+) prefix)
  (declare (type array-index length)
           (type simple-base-string alphabet))
  (assert (or (null prefix)
              (< (length prefix) length)))
  (loop
     :with result = (make-string length :element-type 'base-char)
     :with alphabet-length = (length alphabet)
     :initially (when prefix
                  (replace result prefix))
     :for i :from (if prefix (length prefix) 0) :below length
     :do (setf (aref result i) (aref alphabet (random alphabet-length)))
     :finally (return result)))

(def (function io) new-random-hash-table-key (hash-table key-length &key prefix)
  (iter (for key = (random-simple-base-string key-length +ascii-alphabet+ prefix))
        (for (values value foundp) = (gethash key hash-table))
        (when (not foundp)
          (return key))))

(def (function io) insert-with-new-random-hash-table-key (hash-table value key-length &key prefix)
  (bind ((key (new-random-hash-table-key hash-table key-length :prefix prefix)))
    (setf (gethash key hash-table) value)
    (values key value)))

(def constant +number-of-integer-strings-to-cache+ 128)

(def (constant :test 'equalp) +cached-integer-names+
    (coerce (iter (for idx :from 0 :below +number-of-integer-strings-to-cache+)
                  (collect (coerce (princ-to-string idx) 'simple-base-string)))
            `(simple-array string (,+number-of-integer-strings-to-cache+))))

(def (function io) %integer-to-string (integer &key minimum-column-count (maximum-digit-count most-positive-fixnum) (divisor 10))
  (declare (type integer integer)
           (type (or null fixnum) minimum-column-count)
           (type fixnum maximum-digit-count))
  (if (< integer +number-of-integer-strings-to-cache+)
      (aref +cached-integer-names+ integer)
      (bind ((remainder integer)
             (digit 0)
             (number-of-digits 0)
             (digits (list))
             (result-index 0)
             (result (make-array 128 :element-type 'base-char)))
        (declare (dynamic-extent digits result)
                 (type fixnum digit number-of-digits result-index))
        (macrolet ((emit (char)
                     `(progn
                        (setf (aref result result-index) ,char)
                        (incf result-index))))
          (iter (repeat maximum-digit-count)
                (setf (values remainder digit) (truncate remainder divisor))
                (push digit digits)
                (incf number-of-digits)
                (until (zerop remainder)))
          (when minimum-column-count
            (bind ((padding-length (- minimum-column-count number-of-digits)))
              (when (plusp padding-length)
                (iter (repeat padding-length)
                      (emit #\0)))))
          (dolist (digit digits)
            (emit (code-char (+ #x30 digit)))))
        (bind ((real-result (make-array result-index :element-type 'base-char)))
          (replace real-result result :end1 result-index)
          real-result))))

(def (function io) integer-to-string (integer &key minimum-column-count (maximum-digit-count most-positive-fixnum) (divisor 10))
  (declare (type integer integer)
           (type (or null fixnum) minimum-column-count)
           (type fixnum maximum-digit-count))
  (etypecase integer
    (fixnum (%integer-to-string integer
                                :minimum-column-count minimum-column-count
                                :maximum-digit-count maximum-digit-count
                                :divisor divisor))
    (integer (%integer-to-string integer
                                 :minimum-column-count minimum-column-count
                                 :maximum-digit-count maximum-digit-count
                                 :divisor divisor))))

(declaim (notinline integer-to-string))

;;;;;;;;;;;;;;;;;;;;
;;; xhtml generation

(def (macro e) with-html-stream (stream &body body)
  `(bind ((*html-stream* ,stream))
     ,@body))

(def (macro e) emit-into-html-stream (stream &body body)
  `(bind ((*html-stream* ,stream))
     (emit (progn
             ,@body))))

(def (macro e) emit-into-html-stream-buffer (&body body)
  (with-unique-names (buffer)
    `(with-output-to-sequence (,buffer :external-format +external-format+)
       (bind ((*html-stream* ,buffer))
         (emit (progn
                 ,@body))))))

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

(def (macro e) emit-simple-html-document-response ((&key title status headers cookies) &body body)
  `(emit-http-response* ((append
                          ,@(when headers (list headers))
                          ,@(when status `((list (cons +header/status+ ,status))))
                          '((#.+header/content-type+ . #.+utf-8-html-content-type+)))
                         ,cookies)
     (with-html-document (:content-type +html-content-type+ :title ,title)
       ,@body)))


;;;;;;
;;; Dynamic classes

(def special-variable *dynamic-classes* (make-hash-table :test #'equal))

(def function find-dynamic-class (class-names)
  (assert (every-type-p 'symbol class-names))
  (gethash class-names *dynamic-classes*))

(def function (setf find-dynamic-class) (class class-names)
  (assert (every-type-p 'symbol class-names))
  (setf (gethash class-names *dynamic-classes*) class))

(def function dynamic-class-name (class-names)
  (format-symbol *package* "~{~A~^-~}" class-names))

(def function dynamic-class-metaclass (class-names)
  (bind ((metaclasses
          (mapcar (lambda (class-name)
                    (class-of (find-class class-name)))
                  class-names)))
    (every (lambda (metaclass)
             (eq metaclass (first metaclasses)))
           metaclasses)
    (first metaclasses)))

(def function ensure-dynamic-class (class-names)
  (ensure-class (dynamic-class-name class-names) :direct-superclasses class-names :metaclass (dynamic-class-metaclass class-names)))

(def method make-instance ((class-names list) &rest args)
  (bind ((class (find-dynamic-class class-names)))
    (unless class
      (setf class (ensure-dynamic-class class-names))
      (setf (find-dynamic-class class-names) class))
    (apply #'make-instance class args)))

;;;;;
;;; Hash set

(def function make-hash-set-from-list (elements &key (test #'eq) (key #'identity))
  (ensure-functionf key test)
  (prog1-bind set
      (make-hash-table :test test)
    (dolist (element elements)
      (setf (gethash (funcall key element) set) element))))
