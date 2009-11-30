;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Linear mapping

(def class* linear-mapper ()
  ((predicate :type function)
   (key :type t)
   (value :type t)))

(def class* linear-mapping ()
  ((mappers nil :type list)
   (comparator :type function)
   (cache (make-hash-table :test #'equal) :type hash-table)))

(def print-object linear-mapper
  (format t "~A : ~A" (key-of -self-) (value-of -self-)))

(def function linear-mapping-value (mapping key)
  (bind (((:read-only-slots mappers cache) mapping))
    (or (gethash key cache)
        (setf (gethash key cache)
              (value-of (first (sort (iter (for mapper :in mappers)
                                           (when (funcall (predicate-of mapper) key)
                                             (collect mapper :into result))
                                           (finally
                                            (return (or result
                                                        (error "No mapper found for key ~A in ~A" key mapping)))))
                                     (lambda (mapper-1 mapper-2)
                                       (< (funcall (comparator-of mapping) mapper-1 mapper-2) 0)))))))))

(def function insert-mapper (mapping mapper)
  (bind (((:slots mappers cache) mapping))
    (pushnew mapper mappers :test (lambda (mapper-1 mapper-2)
                                    (zerop (funcall (comparator-of mapping) mapper-1 mapper-2))))
    (clrhash cache)))

;;;;;;
;;; Linear type mapping

(def class* finite-type-mapper (linear-mapper)
  ((key nil :allocation :class)
   (count-limit :type integer)))

(def function insert-subtype-mapper (mapping type value)
  (insert-mapper mapping
                 (make-instance 'linear-mapper
                                :key type
                                :value value
                                :predicate (lambda (key)
                                             (subtypep key type)))))

(def function insert-finite-type-mapper (mapping count-limit value)
  (insert-mapper mapping
                 (make-instance 'finite-type-mapper
                                :value value
                                :count-limit count-limit
                                :predicate (lambda (key)
                                             (awhen (type-instance-count-upper-bound key)
                                               (<= it count-limit))))))

(def generic type-mapper-instance-count-upper-bound (mapper)
  (:method ((mapper linear-mapper))
    (type-instance-count-upper-bound (key-of mapper)))

  (:method ((mapper finite-type-mapper))
    (count-limit-of mapper)))

(def function type-mapper-comparator (mapper-1 mapper-2)
  (bind ((upper-bound-1 (type-mapper-instance-count-upper-bound mapper-1))
         (upper-bound-2 (type-mapper-instance-count-upper-bound mapper-2))
         (key-1 (key-of mapper-1))
         (key-2 (key-of mapper-2))
         (subtypep-1-2? (subtypep key-1 key-2))
         (subtypep-2-1? (subtypep key-2 key-1)))
    (cond ((and (equal upper-bound-1 upper-bound-2)
                (and subtypep-1-2?
                     subtypep-2-1?))
           0)
          ((and upper-bound-1
                upper-bound-2
                (not (zerop (- upper-bound-1 upper-bound-2))))
           (- upper-bound-1 upper-bound-2))
          ((or (and upper-bound-1
                    (not upper-bound-2))
               subtypep-1-2?)
           -1)
          ((or (and upper-bound-2
                    (not upper-bound-1))
               subtypep-2-1?)
           1)
          (t
           ;; NOTE: unrelated types are not equal and are ordered randomly
           (- (sxhash key-1) (sxhash key-2))))))

(def function make-linear-type-mapping ()
  (make-instance 'linear-mapping :comparator 'type-mapper-comparator))

(def definer subtype-mapper (mapping type value)
  `(insert-subtype-mapper ,mapping ',type ',value))

(def definer finite-type-mapper (mapping count value)
  `(insert-finite-type-mapper ,mapping ,count ',value))


#|
(def function test-it ()
  (dolist (mapper-1 (mappers-of *inspector-type-mapping*))
    (dolist (mapper-2 (mappers-of *inspector-type-mapping*))
      (dolist (mapper-3 (mappers-of *inspector-type-mapping*))
        (bind ((result-1-2 (type-mapper-comparator mapper-1 mapper-2))
               (result-1-3 (type-mapper-comparator mapper-1 mapper-3))
               (result-2-3 (type-mapper-comparator mapper-2 mapper-3)))
          (when (and (<= result-1-2 0)
                     (<= result-2-3 0))
            (unless (<= result-1-3 0)
              (format t "~A ~A ~A~%~A ~A ~A~%~%"
                      mapper-1 mapper-2 mapper-3
                      result-1-2 result-2-3 result-1-3)
              (break))))))))


(defun subtype-comparator (type-1 type-2)
  (let* ((subtypep-1-2? (subtypep type-1 type-2))
         (subtypep-2-1? (subtypep type-2 type-1)))
    (cond ((and subtypep-1-2?
                subtypep-2-1?)
           0)
          (subtypep-1-2?
           -1)
          (subtypep-2-1?
           1)
          (t
           0))))

(defun subtype-predicate (type-1 type-2)
  (< (subtype-comparator type-1 type-2) 0))

(defun test-it ()
  (let* ((types (copy-seq '(bit symbol pathname string function keyword)))
         (sorted-types (sort types 'subtype-predicate)))
    (assert (< (position 'keyword sorted-types)
               (position 'symbol sorted-types)))))
|#