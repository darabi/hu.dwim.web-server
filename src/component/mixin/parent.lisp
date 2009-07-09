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
    ;; TODO: can't use this type there, because it would make this slot a component-slot, which is not what we want
    ;;       we could introduce a :child-component slot option but that is somewhat difficult due to CLOS being unable
    ;;       to easily modify slot options during making the direct slots and thus resulting in an initarg error
    #+nil(or null component))))

(def method (setf slot-value-using-class) :after (child (class component-class) (parent component) (slot component-effective-slot-definition))
  (setf (parent-component-references parent) child))

(def (function o) (setf parent-component-references) (child parent &optional parent-component-slot-index)
  (flet (((setf parent-component) (child)
           (bind ((current-parent (parent-component-of child)))
             (assert (or (not current-parent)
                         (eq current-parent parent)) nil "The child ~A is already under another parent" child current-parent))
           (if parent-component-slot-index
               (setf (standard-instance-access child parent-component-slot-index) parent)
               (setf (parent-component-of child) parent))))
    (typecase child
      (parent/mixin
       (setf (parent-component) child))
      (list
       (dolist (element child)
         (when (typep element 'parent/mixin)
           (setf (parent-component) element))))
      (hash-table
       (iter (for (key value) :in-hashtable child)
             (when (typep value 'parent/mixin)
               (setf (parent-component) value))
             (when (typep key 'parent/mixin)
               (setf (parent-component) key)))))))

(def method child-component-slot? ((self parent/mixin) (slot standard-effective-slot-definition))
  (and (not (eq (slot-definition-name slot) 'parent-component))
       (call-next-method)))

(def method add-component-message ((component parent/mixin) message message-args &rest initargs)
  (apply #'add-component-message (parent-component-of component) message message-args initargs))

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
