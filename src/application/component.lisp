;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def special-variable *component* nil
  "The current component where RENDER is going through")

(def definer component (name supers slots &rest options)
  `(def class* ,name ,supers
     ,slots
     (:export-class-name-p #t)
     (:metaclass component-class)
     ,@options))

(def class* component ()
  ((parent-component nil)))

(def (generic e) render (component)
  (:method :around ((component component))
    (bind ((*component* component))
      (call-next-method))))

(def (type e) components ()
  'sequence)

(def method print-object ((self component) stream)
  (bind ((*print-level* nil)
         (*standard-output* stream))
    (handler-bind ((error (lambda (error)
                            (declare (ignore error))
                            (write-string "<<error printing component>>")
                            (return-from print-object))))
      (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
        (pprint-indent :current 1 stream)
        (iter (with class = (class-of self))
              (with class-name = (symbol-name (class-name class)))
              (initially (princ class-name))
              (for slot :in (class-slots class))
              (when (bound-child-component-slot-p class self slot)
                (bind ((initarg (first (slot-definition-initargs slot)))
                       (value (slot-value-using-class class self slot)))
                  (write-char #\Space)
                  (pprint-newline :fill)
                  (prin1 initarg)
                  (write-char #\Space)
                  (pprint-newline :fill)
                  (prin1 value))))))))

(def (function io) bound-child-component-slot-p (class instance slot)
  (and (typep slot 'component-effective-slot-definition)
       (not (eq (slot-definition-name slot) 'parent-component))
       (slot-boundp-using-class class instance slot)))

(def function find-ancestor-component (component predicate)
  (find-ancestor component #'parent-component-of predicate))

(def function find-ancestor-component-with-type (component type)
  (find-ancestor-component component (lambda (component)
                                       (typep component type))))

(def function map-child-components (component thunk)
  (iter (with class = (class-of component))
        (for slot :in (class-slots class))
        (when (bound-child-component-slot-p class component slot)
          (bind ((value (slot-value-using-class class component slot)))
            (typecase value
              (component
               (funcall thunk value))
              (list
               (dolist (element value)
                 (when (typep element 'component)
                   (funcall thunk element)))))))))

(def function map-descendant-components (component thunk &key (include-self #f))
  (labels ((traverse (parent-component)
             (map-child-components parent-component
                                   (lambda (child-component)
                                     (funcall thunk child-component)
                                     (traverse child-component)))))
    (when include-self
      (funcall thunk component))
    (traverse component)))

(def function map-ancestor-components (component thunk &key (include-self #f))
  (labels ((traverse (current)
             (awhen (parent-component-of current)
               (funcall thunk it)
               (traverse it))))
    (when include-self
      (funcall thunk component))
    (traverse component)))

(def function collect-component-ancestors (component &key (include-self #f))
  (nconc
   (when include-self
     (list component))
   (iter (for parent :first component :then (parent-component-of parent))
         (while parent)
         (collect parent))))


;;;;;;
;;; MOP

(def (class e) component-class (standard-class)
  ())

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

(def method (setf slot-value-using-class) :after (new-value (class component-class) (instance component) (slot component-effective-slot-definition))
  (macrolet ((setf-parent (child)
               ;; TODO: (assert (not (parent-component-of child)) nil "The ~A is already under a parent" child)
               `(setf (parent-component-of ,child) instance)))
    (typecase new-value
      (component
       (setf-parent new-value))
      (list
       (dolist (element new-value)
         (when (typep element 'component)
           (setf-parent element))))
      (hash-table
       (iter (for (key value) :in-hashtable new-value)
             (when (typep value 'component)
               (setf-parent value))
             (when (typep key 'component)
               (setf-parent key)))))))

;;; Ensure standard-component is among the supers of the instances of component-class
(def function initialize-or-reinitialize-component-class (class direct-superclasses next-method initargs)
  (declare (dynamic-extent initargs))
  (if (loop :for class :in direct-superclasses
            :thereis (ignore-errors
                       (subtypep class (find-class 'component))))
      (funcall next-method)
      (apply next-method
             class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'component)))
             initargs)))

(def method initialize-instance :around ((class component-class) &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (initialize-or-reinitialize-component-class class direct-superclasses #'call-next-method initargs))

(defmethod reinitialize-instance :around ((class component-class) &rest initargs
                                          &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      (initialize-or-reinitialize-component-class class direct-superclasses #'call-next-method initargs)
      ;; if direct superclasses are not explicitly passed we _must_ not change anything
      (call-next-method)))
