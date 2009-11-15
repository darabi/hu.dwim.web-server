;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Computation, delay and force

(def class* computation ()
  ()
  (:metaclass funcallable-standard-class))

(def (macro e) delay (&body forms)
  (with-unique-names (computation)
    `(delay* (:class-name computation :variable-name ,computation)
       ,@forms)))

(def (macro e) delay* ((&key class-name variable-name) &body forms)
  (if (and (length= 1 forms)
           (not (consp (first forms)))
           (constantp (first forms)))
      (first forms)
      `(bind ((,variable-name (make-instance ',class-name)))
         (set-funcallable-instance-function ,variable-name (named-lambda delay-body () ,@forms))
         ,variable-name)))

(def (function e) force (value)
  (if (typep value 'computation)
      (funcall value)
      value))

(def (function e) computation? (thing)
  (typep thing 'computation))

(def class* one-time-computation (computation)
  ((value :type t))
  (:metaclass funcallable-standard-class))

(def (macro e) one-time-delay (&body forms)
  (with-unique-names (computation)
    `(delay* (:class-name one-time-computation :variable-name ,computation)
       (if (slot-boundp ,computation 'value)
           (value-of ,computation)
           (setf (value-of ,computation)
                 (progn
                   ,@forms))))))

;;;;;;
;;; Utils

(def function string-to-lisp-boolean (value)
  (eswitch (value :test #'string=)
    ("true" #t)
    ("false" #f)))

(def function instance-class-name-as-string (instance)
  (class-name-as-string (class-of instance)))

(def function class-name-as-string (class)
  (string-downcase (symbol-name (class-name class))))

(def (function io) maybe-make-xml-attribute (name value)
  (when value
    (make-xml-attribute name value)))

(def macro notf (&rest places)
  `(setf ,@(iter (for place in places)
                 (collect place)
                 (collect `(not ,place)))))

(def (macro e) foreach (function first-list &rest more-lists)
  `(map nil ,function ,first-list ,@more-lists))

(def function join-strings (strings &key (separator #\Space))
  (with-output-to-string (*standard-output*)
    (iter (for el :in-sequence strings)
          (unless (first-time-p)
            (princ separator))
          (write-string el))))

(def macro to-boolean (form)
  `(not (not ,form)))

(def macro surround-body-when (test surround-with &body body)
  (cond
    ((eq test t)
     `(macrolet ((body ()
                   `(progn ,',@body)))
        (,@surround-with)))
    ((null test)
     `(progn
        ,@body))
    (t `(flet ((-body- () (progn ,@body)))
          (declare (inline -body-) (dynamic-extent #'-body-))
          (if ,test
              (,@surround-with)
              (-body-))))))

(def function mandatory-argument ()
  (error "A mandatory argument was not specified"))

(def function function-name (function)
  (etypecase function
    (generic-function (generic-function-name function))
    (function (sb-impl::%fun-name function))))

(def function find-type-by-name (name &key (otherwise :error))
  (or (find-class name #f)
      (handle-otherwise otherwise)))

(def (function i) class-prototype (class)
  (cond
    ;; KLUDGE: SBCL's class prototypes for built in classes are wrong in some cases
    ((subtypep class 'float)
     42.0)
    ((subtypep class 'string)
     "42")
    ((subtypep class 'list)
     '(42))
    ((subtypep class 'array)
     #(42))
    ((subtypep class 'sequence)
     '(42))
    (t (aprog1 (closer-mop:class-prototype (ensure-finalized class))
         (assert (or (subtypep class 'number)
                     (not (eql it 42))))))))

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

(def function optional-list* (&rest elements)
  (remove nil (apply #'list* elements)))

(def function the-only-element (elements)
  (assert (length= 1 elements))
  (elt elements 0))

(def function filter (element list &key (key #'identity) (test #'eq))
  (remove element list :key key :test-not test))

(def function filter-if (predicate list &key (key #'identity))
  (remove-if (complement predicate) list :key key))

(def function filter-slots (names slots)
  (when names
    (filter-if (lambda (slot)
                 (member (slot-definition-name slot) names))
               slots)))

(def function remove-slots (names slots)
  (if names
      (remove-if (lambda (slot)
                   (member (slot-definition-name slot) names))
                 slots)
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

(deftype simple-ub8-vector (&optional (length '*))
  `(simple-array (unsigned-byte 8) (,length)))

(def (function i) coerce-to-simple-ub8-vector (vector &optional (length (length vector)))
  (if (and (typep vector 'simple-ub8-vector)
           (= length (length vector)))
      vector
      (aprog1
          (make-array length :element-type '(unsigned-byte 8))
        (replace it vector :end2 length))))

(def generic hash-key-for (instance)
  (:method ((instance standard-object))
    instance))

(def function assert-file-exists (file)
  (assert (cl-fad:file-exists-p file))
  file)

(def function append-file-write-date-to-uri (uri &optional file-name)
  (if file-name
      (bind ((*print-pretty* #f)
             (value (mod (file-write-date file-name) 10000)))
        (etypecase uri
          (uri (setf (uri-query-parameter-value uri +timestamp-parameter-name+) value))
          ;; TODO this is not correct, but parsing the uri string is not such a good idea here either... decide.
          (string (string+ uri "?" +timestamp-parameter-name+ "=" (princ-to-string value)))))
      uri))

(def (function e) substitute-illegal-characters-in-file-name (name &key (replacement "_"))
  (cl-ppcre:regex-replace-all "/" name replacement))

(def function single-argument-layered-method-definer (name default-layer forms options)
  (bind ((layer (if (member (first forms) '(:in-layer :in))
                    (progn
                      (pop forms)
                      (pop forms))
                    default-layer))
         (qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms)))
    `(def (layered-method ,@options) ,name ,@(when layer `(:in ,layer)) ,@(when qualifier (list qualifier)) ((-self- ,type))
          ,@forms)))

(declaim (ftype (function () double-float) get-monotonic-time))
(def (function eio) get-monotonic-time ()
  "Returns a time in seconds as a double-float that constantly grows (unaffected by setting the system clock)."
  (isys:%sys-get-monotonic-time))

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

(def generic encoding-name-of (thing)
  (:method ((self external-format))
    (encoding-name-of (external-format-encoding self)))

  (:method ((self babel-encodings:character-encoding))
    (babel-encodings:enc-name self)))

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

(def macro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(def (function i) is-lock-held? (lock)
  #+sbcl (eq (sb-thread::mutex-value lock) (current-thread))
  #-sbcl #t)

(def function mailto-href (email-address)
  (concatenate 'string "mailto:" email-address))

;;;;;;
;;; String utils

(def constant +lower-case-ascii-alphabet+ (coerce "abcdefghijklmnopqrstuvwxyz" 'simple-base-string))
(def constant +upper-case-ascii-alphabet+ (coerce "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'simple-base-string))
(def constant +ascii-alphabet+ (coerce (concatenate 'string +upper-case-ascii-alphabet+ +lower-case-ascii-alphabet+) 'simple-base-string))
(def constant +alphanumeric-ascii-alphabet+ (coerce (concatenate 'string +ascii-alphabet+ "0123456789") 'simple-base-string))
(def constant +base64-alphabet+ (coerce (concatenate 'string +alphanumeric-ascii-alphabet+ "+/") 'simple-base-string))

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
  (check-type length array-index)
  (check-type alphabet simple-base-string)
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

;;;;;;
;;; XML emitting

(def (special-variable e :documentation "The stream for quasi quoted XML output. It is written as a side effect when evaluating quasi quoted XML forms.")
  *xml-stream*)

(def (macro e) with-xml-stream (stream &body body)
  `(bind ((*xml-stream* ,stream))
     ,@body))

(def (macro e) emit-into-xml-stream (stream &body body)
  `(bind ((*xml-stream* ,stream))
     (emit (progn ,@body))))

(def (macro e) emit-into-xml-stream-buffer (&body body)
  (with-unique-names (buffer)
    `(with-output-to-sequence (,buffer :external-format +default-external-format+)
       (bind ((*xml-stream* ,buffer))
         (emit (progn ,@body))))))

;;;;;;
;;; JavaScript emitting

(def (special-variable e :documentation "The stream for quasi quoted JavaScript output. It is written as a side effect when evaluating quasi quoted JavaScript forms.")
  *js-stream*)

(def (macro e) with-js-stream (stream &body body)
  `(bind ((*js-stream* ,stream))
     ,@body))

(def (macro e) emit-into-js-stream (stream &body body)
  `(bind ((*js-stream* ,stream))
     (emit (progn ,@body))))

(def (macro e) emit-into-js-stream-buffer (&body body)
  (with-unique-names (buffer)
    `(with-output-to-sequence (,buffer :external-format +default-external-format+)
       (bind ((*js-stream* ,buffer))
         (emit (progn ,@body))))))

;;;;;;
;;; Debug on error

(def class* debug-context-mixin ()
  ((debug-on-error :type boolean :accessor nil)))

(def method debug-on-error? ((context debug-context-mixin) error)
  (if (slot-boundp context 'debug-on-error)
      (slot-value context 'debug-on-error)
      (call-next-method)))

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

(def special-variable *temporary-file-random-state* (make-random-state t))
(def special-variable *temporary-file-unique-number* 0)

(def special-variable *base-directory-for-temporary-files* (string+ (iolib.pathnames:file-path-namestring iolib.os:*temporary-directory*) "/wui")
  "Used for uploading files among other things.")

(def special-variable *directory-for-temporary-files* nil
  "Holds the runtime value of the temporary directory, which includes the PID. Lazily initialized to be able to extract the PID.")

(def (function e) directory-for-temporary-files ()
  (or *directory-for-temporary-files*
      (setf *directory-for-temporary-files*
            (ensure-directories-exist
             (string+ *base-directory-for-temporary-files*
                      "/"
                      (integer-to-string (isys:%sys-getpid))
                      "/")))))

(def (function e) delete-directory-for-temporary-files ()
  (when *directory-for-temporary-files*
    (iolib.os:delete-files *directory-for-temporary-files* :recursive #t)
    (setf *directory-for-temporary-files* nil)))

(def function filename-for-temporary-file (&optional (prefix nil prefix-provided?))
  (string+ (directory-for-temporary-files)
           prefix
           (when prefix-provided?
             "-")
           ;; TODO atomic-incf
           (integer-to-string (incf *temporary-file-unique-number*))
           "-"
           (integer-to-string (random 100000 *temporary-file-random-state*))))

(def function open-temporary-file (&rest args &key
                                         (element-type '(unsigned-byte 8))
                                         (direction :output)
                                         name-prefix)
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

(def macro ensure-instance (place type &rest args &key &allow-other-keys)
  `(aif ,place
        (reinitialize-instance it ,@args)
        (setf ,place (make-instance ,type ,@args))))

;;;;;
;;; Hash set

(def function make-hash-set-from-list (elements &key (test #'eq) (key #'identity))
  (ensure-functionf key test)
  (prog1-bind set
      (make-hash-table :test test)
    (dolist (element elements)
      (setf (gethash (funcall key element) set) element))))
