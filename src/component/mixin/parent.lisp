;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Parent mixin

(def (component e) parent/mixin ()
  ((parent-component
    nil
    :type t
    :accessor nil
    ;; TODO: can't use this type there, because it would make this slot a component-slot, which is not what we want
    ;;       we could introduce a :child-component slot option but that is somewhat difficult due to CLOS being unable
    ;;       to easily modify slot options during making the direct slots and thus resulting in an initarg error
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

(def method make-component-place ((component parent/mixin))
  (assert component nil "The value NIL is not a valid component.")
  (when-bind parent (parent-component-of component)
    (bind ((parent-class (class-of parent)))
      (iter (for slot :in (class-slots parent-class))
            (when (and (child-component-slot? parent slot)
                       (slot-boundp-using-class parent-class parent slot))
              (bind ((slot-value (slot-value-using-class parent-class parent slot)))
                (typecase slot-value
                  (component
                   (when (eq component slot-value)
                     (return-from make-component-place (make-slot-value-place parent slot))))
                  (list
                   (iter (for index :from 0)
                         (for element :in slot-value)
                         (when (and (typep element 'component)
                                    (eq component element))
                           (return-from make-component-place (make-list-slot-value-place parent slot index))))))))))))

