;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place

(def class* place ()
  ()
  (:documentation "PLACE is a location where data can be stored."))

(def (generic e) place-name (place)
  (:documentation "Returns a symbolic name for PLACE."))

(def (generic e) place-type (place)
  (:documentation "Returns a lisp type specifier for PLACE."))

(def (generic e) place-initform (place)
  (:documentation "Returns a lisp form for PLACE."))

(def (generic e) place-editable? (place)
  (:documentation "TRUE means the PLACE can be edited to refer to other values, otherwise FALSE."))

(def (generic e) place-bound? (place)
  (:documentation "TRUE means the PLACE actually holds a value, otherwise FALSE."))

(def (generic e) make-place-unbound (place)
  (:documentation "Makes the PLACE unbound by removing the value from it."))

(def (generic e) value-at-place (place)
  (:documentation "Returns the current value in PLACE."))

(def (generic e) (setf value-at-place) (new-value place)
  (:documentation "Sets the current value in PLACE to NEW-VALUE."))

(def (generic e) remove-place (place)
  (:documentation "Permanently removes the place, so that it cannot be used to store values anymore."))

;;;;;;
;;; Variable place

(def class* variable-place (place)
  ((name :type symbol)
   (initform :type t)
   (the-type :type t))
  (:documentation "A PLACE for a variable."))

(def method place-name ((self variable-place))
  (name-of self))

(def method place-type ((self variable-place))
  (the-type-of self))

(def method place-initform ((self variable-place))
  (initform-of self))

(def method place-editable? ((self variable-place))
  #t)

;;;;;;
;;; Special variable place

(def class* special-variable-place (variable-place)
  ()
  (:documentation "A PLACE for a special variable."))

(def method place-bound? ((self special-variable-place))
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
  ((getter :type function)
   (setter :type function))
  (:documentation "A PLACE for a lexical variable."))

(def method place-bound? ((self lexical-variable-place))
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
  ((instance :type standard-object)
   (slot :type standard-effective-slot-definition))
  (:documentation "A PLACE for a particular slot of a STANDARD-CLASS instance."))

(def (generic e) slot-value-place-editable? (place class instance slot)
  (:documentation "TRUE means the PLACE can be edited to refer to other values, otherwise FALSE."))

(def generic slot-type (slot)
  (:documentation "TRUE means the PLACE can be edited to refer to other values, otherwise FALSE."))

(def method place-name ((self slot-value-place))
  (slot-definition-name (slot-of self)))

(def method slot-type ((slot standard-slot-definition))
  (slot-definition-type slot))

(def method place-type ((self slot-value-place))
  (slot-type (slot-of self)))

(def method slot-value-place-editable? ((place slot-value-place) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
  #t)

(def method place-editable? ((self slot-value-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-value-place-editable? self class instance (slot-of self))))

(def method place-bound? ((self slot-value-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-boundp-using-class class (instance-of self) (slot-of self))))

(def method make-place-unbound ((self slot-value-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-makunbound-using-class class (instance-of self) (slot-of self))))

(def method value-at-place ((self slot-value-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-value-using-class class (instance-of self) (slot-of self))))

(def method (setf value-at-place) (new-value (self slot-value-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (setf (slot-value-using-class class (instance-of self) (slot-of self)) new-value)))

(def (function e) make-slot-value-place (instance slot)
  (when (symbolp slot)
    (setf slot (find-slot (class-of instance) slot)))
  (make-instance 'slot-value-place :instance instance :slot slot))

;;;;;;
;;; List slot value place

(def class* list-slot-value-place (slot-value-place)
  ((index :type integer))
  (:documentation "A PLACE for a particular slot of a STANDARD-CLASS instance's nth value."))

(def method place-bound? ((self list-slot-value-place))
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

(def method component-at-place ((place place))
  (prog1-bind value
      (value-at-place place)
    (assert (typep value '(or null component)))))

(def method (setf component-at-place) ((replacement-component component) (place place))
  (when-bind replacement-place (make-component-place replacement-component)
    (setf (value-at-place replacement-place) nil))
  (when-bind original-component (value-at-place place)
    (setf (parent-component-of original-component) nil))
  (setf (value-at-place place) replacement-component))

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
