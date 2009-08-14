;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Serialization

(def constant +component-code+ #x61)

(def (function o) deserializer-mapper (code context)
  (cond ((eq code hu.dwim.perec::+persistent-object-by-oid-code+)
         #'hu.dwim.perec::read-persistent-object-by-oid)
        ((eq code +component-code+)
         #'read-component)
        (t (hu.dwim.serializer::default-deserializer-mapper code context))))

(def (function o) serializer-mapper (object context)
  (bind (((:values code has-identity writer-function)
          (hu.dwim.serializer::default-serializer-mapper object context)))
    (if (eq code serializer::+standard-object-code+)
        (typecase object
          (hu.dwim.perec::persistent-object (values hu.dwim.perec::+persistent-object-by-oid-code+ #t #'hu.dwim.perec::write-persistent-object-by-oid))
          (component (values +component-code+ #t #'write-component))
          (t (values code has-identity writer-function)))
        (values code has-identity writer-function))))

;; TODO: split and move to component definition
(def (generic e) collect-component-serialized-slot-names (component)
  (:method-combination append)

  (:method append ((component component))
    '(visible expanded dirty outdated))

  (:method append ((component standard-class/mixin))
    '(the-class))
  
  (:method append ((component standard-slot-definition/mixin))
    '(the-class slot))
  
  (:method append ((component standard-object/mixin))
    '(instance))

  (:method append ((component reference-component))
    '(target))

  (:method append ((component primitive-component))
    '(name the-type component-value))

  (:method append ((component place-filter))
    '(name the-type use-in-filter use-in-filter-id negated selected-predicate)))

(def (function e) write-component (component context)
  (serializer::write-slot-object-slots component context
                                       (append (component-slots-of (class-of component))
                                               (mapcar [find-slot (class-of component) !1] (collect-component-serialized-slot-names component)))))

(def (function e) read-component (context &optional referenced)
  (declare (ignore referenced))
  (prog1-bind component (serializer::read-slot-object-slots context #t)
    (setf (parent-component-of component) nil)))

(def (function e) serialize-component (component)
  (serializer:serialize component :serializer-mapper #'serializer-mapper))

(def (function e) deserialize-component (input)
  (prog1-bind component (serializer:deserialize input :deserializer-mapper #'deserializer-mapper)
    (refresh-component component)))
