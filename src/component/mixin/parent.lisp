;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Parent mixin

(def (component ea) parent/mixin ()
  ((parent-component
    nil
    :type t
    :accessor nil
    ;; TODO: can't use this type there, because it would make this slot a component-slot, which is not what we want
    ;;       introduce a :child-component slot option
    ;;       somewhat difficult due to CLOS being unable to easily modify slot options during making the direct slots
    #+nil(or null component))))

(def method (setf slot-value-using-class) :after (new-value (class component-class) (instance parent/mixin) (slot component-effective-slot-definition))
  (setf (parent-component-references instance) new-value))

(def (function o) (setf parent-component-references) (new-value instance &optional parent-component-slot-index)
  (flet (((setf parent-component) (child)
           (assert (not (parent-component-of child)) nil "The child ~A is already under a parent" child)
           (if parent-component-slot-index
               (setf (standard-instance-access child parent-component-slot-index) instance)
               (setf (parent-component-of child) instance))))
    (typecase new-value
      (component
       (setf (parent-component) new-value))
      (list
       (dolist (element new-value)
         (when (typep element 'component)
           (setf (parent-component) element))))
      (hash-table
       (iter (for (key value) :in-hashtable new-value)
             (when (typep value 'component)
               (setf (parent-component) value))
             (when (typep key 'component)
               (setf (parent-component) key)))))))

(def method child-component-slot? ((self parent/mixin) (slot standard-effective-slot-definition))
  (not (eq (slot-definition-name slot) 'parent-component)))

(def method add-user-message ((component parent/mixin) message message-args &rest initargs)
  (apply #'add-user-message (parent-component-of component) message message-args initargs))

;;;;;;
;;; Parent child relationship

(def (function e) find-ancestor-component (component predicate)
  (find-ancestor component #'parent-component-of predicate))

(def (function e) find-ancestor-component-with-type (component type)
  (find-ancestor-component component [typep !1 type]))

(def (function e) find-descendant-component (component predicate)
  (map-descendant-components component (lambda (child)
                                         (when (funcall predicate child)
                                           (return-from find-descendant-component child)))))

(def (function e) find-descendant-component-with-type (component type)
  (find-descendant-component component [typep !1 type]))

(def (function e) collect-path-to-root-component (component)
  (iter (for ancestor-component :initially component :then (parent-component-of ancestor-component))
        (while ancestor-component)
        (collect ancestor-component)))

(def (function e) map-child-components (component visitor)
  (ensure-functionf visitor)
  (iter (with class = (class-of component))
        (for slot :in (class-slots class))
        (when (and (child-component-slot? component slot)
                   (slot-boundp-using-class class component slot))
          (bind ((value (slot-value-using-class class component slot)))
            (typecase value
              (component
               (funcall visitor value))
              (list
               (dolist (element value)
                 (when (typep element 'component)
                   (funcall visitor element))))
              (hash-table
               (iter (for (key element) :in-hashtable value)
                     (when (typep element 'component)
                       (funcall visitor element)))))))))

(def (function e) map-descendant-components (component visitor &key (include-self #f))
  (ensure-functionf visitor)
  (labels ((traverse (parent-component)
             (map-child-components parent-component
                                   (lambda (child-component)
                                     (funcall visitor child-component)
                                     (traverse child-component)))))
    (when include-self
      (funcall visitor component))
    (traverse component)))

(def (function e) map-ancestor-components (component visitor &key (include-self #f))
  (ensure-functionf visitor)
  (labels ((traverse (current)
             (awhen (parent-component-of current)
               (funcall visitor it)
               (traverse it))))
    (when include-self
      (funcall visitor component))
    (traverse component)))

(def (function e) collect-ancestor-components (component &key (include-self #f))
  (nconc
   (when include-self
     (list component))
   (iter (for parent :first component :then (parent-component-of parent))
         (while parent)
         (collect parent))))
