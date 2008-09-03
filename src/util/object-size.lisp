(in-package :hu.dwim.wui)

;;;;;;
;;; Object size breakdown

(def constant +word-size+ 8)

(def (class* e) object-size-descriptor ()
  ((class-name)
   (count 0)
   (size 0)))

(def print-object object-size-descriptor ()
  (format t "~A ~A : ~A" (class-name-of -self-) (count-of -self-) (size-of -self-)))

(def (function e) describe-object-sizes (object &key ignored-type)
  (bind ((class-name-to-object-size-descriptor-map (make-hash-table :test #'eq))
         (seen-object-set (make-hash-table :test #'eq)))
    (labels ((%describe-object-sizes (object)
               (unless (or (gethash object seen-object-set)
                           (typep object ignored-type))
                 (setf (gethash object seen-object-set) #t)
                 (bind ((class (class-of object))
                        (class-name (class-name class))
                        (descriptor (or (gethash class-name class-name-to-object-size-descriptor-map)
                                        (setf (gethash class-name class-name-to-object-size-descriptor-map)
                                              (make-instance 'object-size-descriptor :class-name class-name))))
                        (size (compute-allocated-size object)))
                   (incf (count-of descriptor))
                   (incf (size-of descriptor) size)
                   ;; TODO: we should support customization here
                   (etypecase object
                     ((or null (eql t) number string)
                      (values))
                     (cons
                      (%describe-object-sizes (car object))
                      (%describe-object-sizes (cdr object)))
                     (symbol
                      (%describe-object-sizes (symbol-name object))
                      (%describe-object-sizes (symbol-package object))
                      (%describe-object-sizes (symbol-plist object))
                      (when (boundp object)
                        (%describe-object-sizes (symbol-value object)))
                      (when (fboundp object)
                        (%describe-object-sizes (symbol-function object))))
                     (hash-table
                      (iter (for (key value) :in-hashtable object)
                            (%describe-object-sizes key)
                            (%describe-object-sizes value)))
                     (array
                      (dotimes (i (apply #'* (array-dimensions object)))
                        (%describe-object-sizes (row-major-aref object i))))
                     (structure-object
                      (dolist (slot (class-slots class))
                        (%describe-object-sizes (slot-value-using-class class object slot))))
                     (standard-object
                      (dolist (slot (class-slots class))
                        (%describe-object-sizes
                         (ignore-errors (standard-instance-access object (slot-definition-location slot))))))
                     (sb-vm::code-component
                      (let ((length (sb-vm::get-header-data object)))
                        (do ((i sb-vm::code-constants-offset (1+ i)))
                            ((= i length))
                          (%describe-object-sizes (sb-vm::code-header-ref object i)))))
                     (function
                      (bind ((widetag (sb-kernel:widetag-of object)))
                        (cond ((= widetag sb-vm:simple-fun-header-widetag)
                               (%describe-object-sizes (sb-kernel:fun-code-header object)))
                              ((= widetag sb-vm:closure-header-widetag)
                               (%describe-object-sizes (sb-kernel:%closure-fun object))
                               (iter (for i :from 0 :below (1- (sb-kernel:get-closure-length object)))
                                     (%describe-object-sizes (sb-kernel:%closure-index-ref object i))))
                              (t (error "Unknown function type ~A" object))))))))))
      (%describe-object-sizes object))
    (hash-table-values class-name-to-object-size-descriptor-map)))

(def function compute-allocated-size (object)
  (etypecase object
    ((or null (eql t) fixnum float)
     ;; these are immediate values
     0)
    (integer
     (+ +word-size+
        (floor (/ (integer-length object) 8 +word-size+))))
    (cons
     (* 2 +word-size+))
    (base-string
     (+ +word-size+
        (length object)))
    (string
     (+ +word-size+
        (* 4 (length object))))
    (symbol
     ;; (package name value function plist)
     (* 5 +word-size+))
    (array
     (+ +word-size+
        (* (eswitch ((array-element-type object) :test #'equal)
             ('(unsigned-byte 8) 1)
             ('(unsigned-byte 16) 2)
             ('t +word-size+))
           (array-total-size object))))
    ((or structure-object standard-object)
     (* +word-size+
        (+ 2 (length (class-slots (class-of object))))))
    (function
     (bind ((widetag (sb-kernel:widetag-of object)))
       (cond ((= widetag sb-vm:simple-fun-header-widetag)
              0)
             ((= widetag sb-vm:closure-header-widetag)
              (* +word-size+ (1- (sb-kernel:get-closure-length object))))
             (t (error "Unknown function type ~A" object)))))))
