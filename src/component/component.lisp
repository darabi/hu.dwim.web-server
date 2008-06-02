;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;;;;;;;;
;;; Component

(def (definer e) component (name supers slots &rest options)
  `(def class* ,name ,supers
     ,slots
     (:export-class-name-p #t)
     (:metaclass component-class)
     ,@options))

(def component component (ui-syntax-node)
  ((parent-component nil)))

(def (type e) components ()
  'sequence)

;;;;;;
;;; Parent child relationship

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
