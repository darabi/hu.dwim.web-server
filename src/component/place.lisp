;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place

(def class* place ()
  ())

(def (generic e) place-name (place))

(def (generic e) place-type (place))

(def (generic e) place-initform (place))

(def (generic e) place-editable-p (place))

(def (generic e) place-bound-p (place))

(def (generic e) make-place-unbound (place))

(def (generic e) value-at-place (place))

(def (generic e) (setf value-at-place) (new-value place))

(def (generic e) remove-place (place))

;;;;;;
;;; Variable place

(def class* variable-place (place)
  ((name)
   (initform)
   (the-type)))

(def method place-name ((self variable-place))
  (name-of self))

(def method place-type ((self variable-place))
  (the-type-of self))

(def method place-initform ((self variable-place))
  (initform-of self))

(def method place-editable-p ((self variable-place))
  #t)

;;;;;;
;;; Special variable place

(def class* special-variable-place (variable-place)
  ())

(def method place-bound-p ((self special-variable-place))
  (boundp (name-of self)))

(def method make-place-unbound ((self special-variable-place))
  (makunbound (name-of self)))

(def method value-at-place ((self special-variable-place))
  (symbol-value (name-of self)))

(def method (setf value-at-place) (new-value (self special-variable-place))
  (setf (symbol-value (name-of self)) new-value))

(def (function e) make-special-variable-place (name type)
  (make-instance 'special-variable-place :name name :the-type type))

;;;;;;
;;; Lexical variable place

(def class* lexical-variable-place (variable-place)
  ((getter)
   (setter)))

(def method place-bound-p ((self lexical-variable-place))
  #t)

(def method make-place-unbound ((self lexical-variable-place))
  (error "Cannot make ~A unbound" self))

(def method value-at-place ((self lexical-variable-place))
  (funcall (getter-of self)))

(def method (setf value-at-place) (new-value (self lexical-variable-place))
  (funcall (setter-of self) new-value))

(def (macro e) make-lexical-variable-place (name type)
  `(make-instance 'lexical-variable-place
                  :name ,name
                  :the-type ,type
                  :getter (lambda ()
                            ,name)
                  :setter (lambda (value)
                            (setf ,name value))))

;;;;;;
;;; Slot value place

(def class* slot-value-place (place)
  ((instance)
   (slot)))

(def method place-name ((self slot-value-place))
  (slot-definition-name (slot-of self)))

(def function revive-slot-value-place-instance (place)
  (bind ((instance (instance-of place)))
    (setf (instance-of place) (reuse-standard-object-instance (class-of instance) instance))))

(def generic slot-type (slot)
  (:method ((slot standard-slot-definition))
    (slot-definition-type slot)))

(def method place-type ((self slot-value-place))
  (slot-type (slot-of self)))

(def (generic e) slot-value-place-editable-p (place class instance slot)
  (:method ((place slot-value-place) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
    #t))

(def method place-editable-p ((self slot-value-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-value-place-editable-p self class instance (slot-of self))))

(def method place-bound-p ((self slot-value-place))
  (bind ((instance (revive-slot-value-place-instance self)))
    (slot-boundp-using-class (class-of instance) instance (slot-of self))))

(def method make-place-unbound ((self slot-value-place))
  (bind ((instance (revive-slot-value-place-instance self)))
    (slot-makunbound-using-class (class-of instance) instance (slot-of self))))

(def method value-at-place ((self slot-value-place))
  (bind ((instance (revive-slot-value-place-instance self)))
    (slot-value-using-class (class-of instance) instance (slot-of self))))

(def method (setf value-at-place) (new-value (self slot-value-place))
  (bind ((instance (revive-slot-value-place-instance self)))
    (setf (slot-value-using-class (class-of instance) instance (slot-of self)) new-value)))

(def (function e) make-slot-value-place (instance slot)
  (when (symbolp slot)
    (setf slot (find-slot (class-of instance) slot)))
  (make-instance 'slot-value-place :instance instance :slot slot))

;;;;;;
;;; List slot value place

(def class* list-slot-value-place (slot-value-place)
  ((index)))

(def method place-bound-p ((self list-slot-value-place))
  #t)

(def method make-place-unbound ((self list-slot-value-place))
  (error "Cannot make ~A unbound" self))

(def method value-at-place ((self list-slot-value-place))
  (nth (index-of self) (call-next-method)))

(def method (setf value-at-place) (new-value (self list-slot-value-place))
  (bind ((instance (instance-of self))
         (slot (slot-of self))
         (class (class-of instance)))
    (bind ((list (slot-value-using-class class instance slot)))
      (setf (nth (index-of self) list) new-value)
      (setf (slot-value-using-class class instance slot) list))))

(def method remove-place ((self list-slot-value-place))
  (bind ((instance (instance-of self))
         (slot (slot-of self))
         (class (class-of instance))
         (index (index-of self))
         (value (slot-value-using-class class instance slot)))
    (setf (slot-value-using-class class instance slot)
          (append (subseq value 0 index) (subseq value (1+ index))))))

(def (function e) make-list-slot-value-place (instance slot index)
  (make-instance 'list-slot-value-place :instance instance :slot slot :index index))

;;;;;;
;;; Component place

(def (function e) component-at-place (place)
  (prog1-bind value
      (value-at-place place)
    (assert (typep value '(or null component)))))

(def (function e) (setf component-at-place) (replacement-component place)
  (when-bind replacement-place (make-component-place replacement-component)
    (setf (value-at-place replacement-place) nil))
  (when-bind original-component (value-at-place place)
    (setf (parent-component-of original-component) nil))
  (setf (value-at-place place) replacement-component))

(def (function e) make-component-place (component)
  "Returns the COMPONENT-PLACE or NIL if no such PLACE exists"
  (assert component nil "The value NIL is not a valid component")
  (when-bind parent (parent-component-of component)
    (bind ((parent-class (class-of parent)))
      (iter (for slot :in (class-slots parent-class))
            (when (bound-child-component-slot-p parent-class parent slot)
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
