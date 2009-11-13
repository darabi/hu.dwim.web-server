(in-package :hu.dwim.wui)

;;;;;;
;;; Object size breakdown

(def constant +word-size-in-bytes+
  #+sbcl sb-vm:n-word-bytes
  #-sbcl 8)

(def (class* e) object-size-descriptor ()
  ((class-name)
   (count 0)
   (size 0)))

(def print-object object-size-descriptor ()
  (format t "~A ~A : ~A" (class-name-of -self-) (count-of -self-) (size-of -self-)))

(def (function e) collect-object-size-descriptors-for-retained-objects (root &key ignored-type)
  (bind ((class-name->object-size-descriptor (make-hash-table :test #'eq)))
    (%iterate-descendant-objects
     root
     (lambda (object)
       (bind ((class (class-of object))
              (class-name (class-name class))
              (descriptor (or (gethash class-name class-name->object-size-descriptor)
                              (setf (gethash class-name class-name->object-size-descriptor)
                                    (make-instance 'object-size-descriptor :class-name class-name))))
              (size (object-allocated-size object)))
         (incf (count-of descriptor))
         (incf (size-of descriptor) size)))
     :ignored-type ignored-type
     :mode :retained)
    (hash-table-values class-name->object-size-descriptor)))

(def function %iterate-descendant-objects (root visitor &key ignored-type (mode :retained))
  (check-type mode (member :retained :reachable))
  (bind ((seen-object-set (make-hash-table :test #'eq)))
    (labels ((recurse (object)
               (unless (or (gethash object seen-object-set)
                           (not ignored-type)
                           (typep object ignored-type))
                 (setf (gethash object seen-object-set) #t)
                 (funcall visitor object)
                 (etypecase object
                   ((or number string character)
                    (values))
                   (cons
                    (recurse (car object))
                    (recurse (cdr object)))
                   (symbol
                    (when (eq mode :reachable)
                      (recurse (symbol-name object))
                      (recurse (symbol-package object))
                      (recurse (symbol-plist object))
                      (when (boundp object)
                        (recurse (symbol-value object)))
                      (when (fboundp object)
                        (recurse (symbol-function object))))
                    (recurse (symbol-name object)))
                   (hash-table
                    ;; TODO handle weak hashtables when mode is :retained
                    (iter (for (key value) :in-hashtable object)
                          (recurse key)
                          (recurse value)))
                   (array
                    (dotimes (i (apply #'* (array-dimensions object)))
                      (recurse (row-major-aref object i))))
                   (structure-object
                    (bind ((class (class-of object)))
                      (dolist (slot (class-slots class))
                        (recurse (slot-value-using-class class object slot)))))
                   (standard-object
                    ;; TODO should grab the underlying vector and check for sb-pcl::*unbound-slot-value-marker*
                    (bind ((class (class-of object)))
                      (dolist (slot (class-slots class))
                        (bind ((slot-location (slot-definition-location slot)))
                          (ecase (slot-definition-allocation slot)
                            (:instance (recurse (if (typep object 'funcallable-standard-object)
                                                    (funcallable-standard-instance-access object slot-location)
                                                    (standard-instance-access object slot-location))))
                            (:class))))))
                   #+sbcl
                   (sb-vm::code-component
                    (let ((length (sb-vm::get-header-data object)))
                      (do ((i sb-vm::code-constants-offset (1+ i)))
                          ((= i length))
                        (recurse (sb-vm::code-header-ref object i)))))
                   #+sbcl
                   (sb-kernel::random-class
                    ;; TODO:
                    )
                   #+sbcl
                   (sb-sys:system-area-pointer
                    ;; TODO:
                    )
                   (function
                    #+sbcl
                    (bind ((widetag (sb-kernel:widetag-of object)))
                      (cond ((= widetag sb-vm:simple-fun-header-widetag)
                             (recurse (sb-kernel:fun-code-header object)))
                            ((= widetag sb-vm:closure-header-widetag)
                             (recurse (sb-kernel:%closure-fun object))
                             (sb-impl::do-closure-values (value object)
                               (recurse value)))
                            (t (error "Unknown function type ~A" object)))))))))
      (recurse root))))

(def function object-allocated-size (object)
  #*((:sbcl (object-allocated-size/sbcl object))
     (t (object-allocated-size/generic object))))

(def function compute-allocated-size/generic (object)
  (etypecase object
    ((or null (eql t) fixnum float)
     ;; these are immediate values
     0)
    (integer
     (+ +word-size-in-bytes+
        (floor (/ (integer-length object) 8 +word-size-in-bytes+))))
    (cons
     (* 2 +word-size-in-bytes+))
    (base-string
     (+ +word-size-in-bytes+
        (length object)))
    (string
     (+ +word-size-in-bytes+
        (* 4 (length object))))
    (symbol
     ;; (package name value function plist)
     (* 5 +word-size-in-bytes+))
    (array
     (+ +word-size-in-bytes+
        (* (eswitch ((array-element-type object) :test #'equal)
             ('(unsigned-byte 8) 1)
             ('(unsigned-byte 16) 2)
             ('t +word-size-in-bytes+))
           (array-total-size object))))
    ((or structure-object standard-object)
     (* +word-size-in-bytes+
        (+ 2 (length (class-slots (class-of object))))))
    (function
     ;; KLUDGE this is *wrong*, the whole world could be captured in a closure...
     0)))

#+sbcl
(def function object-allocated-size/sbcl (object)
  (flet ((round-to-dualword (size)
           (logand (the sb-vm:word (+ size sb-vm:lowtag-mask))
                   (lognot sb-vm:lowtag-mask)))
         (sbcl-vector-size (object)
           (sb-vm::vector-total-size object (svref sb-vm::*room-info* (sb-kernel:widetag-of object)))))
    (etypecase object
      ((or fixnum float character)
       ;; these are immediate values
       0)
      (integer
       ;; TODO probably much more
       (+ +word-size-in-bytes+
          (floor (/ (integer-length object) 8 +word-size-in-bytes+))))
      (ratio
       ;; probably something like that
       (+ (* 3 +word-size-in-bytes+)
          (object-allocated-size/sbcl (numerator object))
          (object-allocated-size/sbcl (denominator object))))
      (cons
       (* 2 +word-size-in-bytes+))
      (simple-base-string
       (sbcl-vector-size object))
      (base-string
       (+ +word-size-in-bytes+
          (length object)))
      (simple-string
       (sbcl-vector-size object))
      (string
       (+ +word-size-in-bytes+
          (* 4 (length object))))
      (symbol
       (* sb-vm:symbol-size +word-size-in-bytes+))
      (array
       (+ +word-size-in-bytes+
          (* (eswitch ((array-element-type object) :test #'equal)
               ('(unsigned-byte 8) 1)
               ('(unsigned-byte 16) 2)
               ('t +word-size-in-bytes+))
             (array-total-size object))))
      (funcallable-standard-object
       ;; TODO
       0)
      (function
       (bind ((widetag (sb-kernel:widetag-of object)))
         (cond
           ((= widetag sb-vm:simple-fun-header-widetag)
            0)
           ((= widetag sb-vm:closure-header-widetag)
            (round-to-dualword (* (the fixnum (1+ (sb-kernel:get-closure-length object)))
                                  sb-vm:n-word-bytes)))
           (t (error "Unknown function type ~A" object)))))
      (sb-kernel::random-class
       ;; TODO:
       0)
      ((or structure-object standard-object)
       (round-to-dualword (* (+ (sb-kernel:%instance-length object) 1)
                             sb-vm:n-word-bytes))))))
