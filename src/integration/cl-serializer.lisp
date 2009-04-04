;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Serialization

(def constant +component-code+ #x61)

(def (function o) deserializer-mapper (code context)
  (cond ((eq code cl-perec::+persistent-object-by-oid-code+)
         #'cl-perec::read-persistent-object-by-oid)
        ((eq code +component-code+)
         #'read-component)
        (t (cl-serializer::default-deserializer-mapper code context))))

(def (function o) serializer-mapper (object context)
  (bind (((:values code has-identity writer-function)
          (cl-serializer::default-serializer-mapper object context)))
    (if (eq code serializer::+standard-object-code+)
        (typecase object
          (cl-perec::persistent-object (values cl-perec::+persistent-object-by-oid-code+ #t #'cl-perec::write-persistent-object-by-oid))
          (component (values +component-code+ #t #'write-component))
          (t (values code has-identity writer-function)))
        (values code has-identity writer-function))))

;; TODO: split and move to component definition
(def (generic e) collect-component-serialized-slot-names (component)
  (:method-combination append)

  (:method append ((component component))
    '(visible expanded dirty outdated))

  (:method append ((component abstract-standard-class-component))
    '(the-class))
  
  (:method append ((component abstract-standard-slot-definition-component))
    '(the-class slot))
  
  (:method append ((component abstract-standard-object-component))
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
