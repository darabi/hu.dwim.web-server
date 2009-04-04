;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; MOP

(def (class* e) component-class (computed-class)
  ((computed-slots nil)
   (component-slots nil)))

(def class component-slot-definition (standard-slot-definition)
  ())

(def class component-direct-slot-definition (component-slot-definition standard-direct-slot-definition)
  ())

(def class component-effective-slot-definition (component-slot-definition standard-effective-slot-definition)
  ())

(def method validate-superclass ((class component-class) (superclass standard-class))
  #t)

(def function component-slot? (args)
  (member (getf args :type) '(component components)))

(def method direct-slot-definition-class ((class component-class) &rest args)
  (if (component-slot? args)
      (find-class 'component-direct-slot-definition)
      (call-next-method)))

(def method effective-slot-definition-class ((class component-class) &rest args)
  (if (component-slot? args)
      (find-class 'component-effective-slot-definition)
      (call-next-method)))

;;; Ensure standard-component is among the supers of the instances of component-class
(def function shared-initialize-around-component-class (class class-name direct-superclasses next-method initargs)
  (declare (dynamic-extent initargs))
  (if (or (eq class-name 'component)
          (loop :for class :in direct-superclasses
             :thereis (ignore-errors
                        (subtypep class (find-class 'component)))))
      (funcall next-method)
      (apply next-method
             class
             :direct-superclasses
             (append direct-superclasses
                     (unless (eq (getf initargs :name) 'component)
                       (list (find-class 'component))))
             initargs)))

(def method initialize-instance :around ((class component-class) &rest initargs &key name direct-superclasses)
  (declare (dynamic-extent initargs))
  (shared-initialize-around-component-class class name direct-superclasses #'call-next-method initargs))

(def method reinitialize-instance :around ((class component-class) &rest initargs
                                           &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      (shared-initialize-around-component-class class (class-name class) direct-superclasses #'call-next-method initargs)
      ;; if direct superclasses are not explicitly passed we _must_ not change anything
      (call-next-method)))

(def method finalize-inheritance :after ((class component-class))
  (setf (computed-slots-of class) (filter-if (of-type 'computed-effective-slot-definition) (class-slots class)))
  (setf (component-slots-of class) (filter-if (of-type 'component-effective-slot-definition) (class-slots class))))

;;;;;;
;;; Computed universe

(define-computed-universe compute-as-in-session)

(def function ensure-session-computed-universe ()
  (or (computed-universe-of *session*)
      (setf (computed-universe-of *session*) (make-computed-universe))))

(def (macro e) compute-as (&body forms)
  `(compute-as* ()
     ,@forms))
(setf (get 'compute-as 'cc::computed-as-macro-p) #t)
(setf (get 'compute-as 'cc::primitive-compute-as-macro) 'compute-as-in-session*)

(def (macro e) compute-as* ((&rest args &key &allow-other-keys) &body forms)
  `(compute-as-in-session* (:universe (ensure-session-computed-universe) ,@args)
     (compute-as-body -self- (lambda () ,@forms))))
(setf (get 'compute-as* 'cc::computed-as-macro-p) #t)
(setf (get 'compute-as* 'cc::primitive-compute-as-macro) 'compute-as-in-session*)

(def function compute-as-body (component thunk)
  ;; TODO: is it the right place?
  (mark-outdated component)
  (bind ((result (funcall thunk)))
    (if (typep result 'standard-object)
        (reuse-standard-object-instance (class-of result) result)
        result)))
