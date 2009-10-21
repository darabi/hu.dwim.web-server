;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place

(def (class* e) place ()
  ()
  (:documentation "PLACE is a location where data can be stored at and retrieved from."))

(def (generic e) place-name (place)
  (:documentation "Returns a symbolic name for PLACE."))

(def (generic e) place-documentation (place)
  (:documentation "Returns a documentation string for PLACE."))

(def (generic e) place-type (place)
  (:documentation "Returns a lisp type specifier for PLACE."))

(def (generic e) place-initform (place)
  (:documentation "Returns a lisp initform for PLACE."))

(def (generic e) place-editable? (place)
  (:documentation "TRUE means the PLACE can be edited and set to other values, otherwise FALSE."))

(def (generic e) place-bound? (place)
  (:documentation "TRUE means the PLACE actually holds a value, otherwise FALSE."))

(def (generic e) make-place-unbound (place)
  (:documentation "Makes the PLACE unbound by removing the value from it."))

(def (generic e) value-at-place (place)
  (:documentation "Returns the current value in PLACE."))

(def (generic e) (setf value-at-place) (new-value place)
  (:documentation "Sets the current value in PLACE to NEW-VALUE."))

;;;;;;
;;; basic-place

(def (class* e) basic-place (place)
  ((name :type symbol)
   (initform :type t)
   (the-type t :type t))
  (:documentation "An abstract basic PLACE."))

(def method place-name ((self basic-place))
  (name-of self))

(def method place-documentation ((self basic-place))
  nil)

(def method place-type ((self basic-place))
  (the-type-of self))

(def method place-initform ((self basic-place))
  (initform-of self))

(def method place-editable? ((self basic-place))
  #t)

(def method place-bound? ((self basic-place))
  #t)

(def method make-place-unbound ((self basic-place))
  (error "Cannot make ~A unbound" self))

;;;;;;
;;; functional-place

(def (class* e) functional-place (basic-place)
  ((getter :type (or symbol function))
   (setter :type (or symbol function)))
  (:documentation "A PLACE that is get and set by using lambda functions."))

(def method value-at-place ((self functional-place))
  (funcall (getter-of self)))

(def method (setf value-at-place) (new-value (self functional-place))
  (funcall (setter-of self) new-value))

(def (function e) make-functional-place (name getter setter &key (type t))
  (check-type name symbol)
  (make-instance 'functional-place
                 :name name
                 :getter getter
                 :setter setter
                 :the-type type))

;;;;;;
;;; simple-functional-place

(def (class* e) simple-functional-place (functional-place)
  ((argument :type t))
  (:documentation "A PLACE that is get and set by using lambda functions on a single argument."))

(def method value-at-place ((self simple-functional-place))
  (funcall (getter-of self) (argument-of self)))

(def method (setf value-at-place) (new-value (self simple-functional-place))
  (funcall (setter-of self) new-value (argument-of self)))

(def (function e) make-simple-functional-place (argument name &key (type t))
  (check-type name symbol)
  (make-instance 'simple-functional-place
                 :argument argument
                 :name name
                 :getter (fdefinition name)
                 :setter (fdefinition `(setf ,name))
                 :the-type type))

;;;;;;
;;; variable-place

(def (class*) variable-place (basic-place)
  ()
  (:documentation "An abstract PLACE for a variable."))

;;;;;;
;;; special-variable-place

(def (class*) special-variable-place (variable-place)
  ()
  (:documentation "A PLACE for a special variable."))

(def method place-documentation ((self special-variable-place))
  (documentation (name-of self) 'variable))

(def method place-bound? ((self special-variable-place))
  (boundp (name-of self)))

(def method make-place-unbound ((self special-variable-place))
  (makunbound (name-of self)))

(def method value-at-place ((self special-variable-place))
  (symbol-value (name-of self)))

(def method (setf value-at-place) (new-value (self special-variable-place))
  (setf (symbol-value (name-of self)) new-value))

(def (function e) make-special-variable-place (name &key (type t))
  (check-type name symbol)
  (make-instance 'special-variable-place :name name :the-type type))

;;;;;;
;;; lexical-variable-place

(def (class* e) lexical-variable-place (variable-place functional-place)
  ()
  (:documentation "A PLACE for a lexical variable."))

(def (macro e) make-lexical-variable-place (name &key (type t))
  (check-type name symbol)
  `(make-instance 'lexical-variable-place
                  :name ',name
                  :the-type ,type
                  :getter (lambda ()
                            ,name)
                  :setter (lambda (value)
                            (setf ,name value))))

;;;;;;
;;; sequence-element-place

(def (class* e) sequence-element-place (basic-place)
  ((sequence :type sequence)
   (index :type integer))
  (:documentation "A PLACE for an element of a sequence."))

(def method place-type ((self sequence-element-place))
  (bind ((sequence (sequence-of self)))
    (if (consp sequence)
        t
        (array-element-type sequence))))

(def method value-at-place ((self sequence-element-place))
  (elt (sequence-of self) (index-of self)))

(def method (setf value-at-place) (new-value (self sequence-element-place))
  (setf (elt (sequence-of self) (index-of self)) new-value))

(def (function e) make-sequence-element-place (sequence index)
  (check-type sequence sequence)
  (check-type index integer)
  (make-instance 'sequence-element-place
                 :sequence sequence
                 :index index
                 :name index))

;;;;;;
;;; object-slot-place

(def (class* e) object-slot-place (place)
  ((instance :type standard-object)
   (slot :type standard-effective-slot-definition))
  (:documentation "A PLACE for a particular slot of an object instance."))

(def (generic e) object-slot-place-editable? (place class instance slot)
  (:documentation "TRUE means the PLACE can be edited and set to other values, otherwise FALSE.")

  (:method  ((place object-slot-place) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
    #t))

(def generic slot-type (class prototype slot)
  (:documentation "Returns a lisp type specifier for SLOT.")

  (:method (class prototype (slot slot-definition))
    (slot-definition-type slot))

  #+sbcl
  (:method (class prototype (slot sb-pcl::structure-slot-definition))
    (slot-definition-type slot)))

(def method place-name ((self object-slot-place))
  (slot-definition-name (slot-of self)))

(def method place-documentation ((self object-slot-place))
  (documentation (slot-of self) t))

(def method place-type ((self object-slot-place))
  (bind ((class (class-of (instance-of self))))
    (slot-type class (class-prototype class) (slot-of self))))

(def method place-editable? ((self object-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (object-slot-place-editable? self class instance (slot-of self))))

(def method place-bound? ((self object-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-boundp-using-class class (instance-of self) (slot-of self))))

(def method make-place-unbound ((self object-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-makunbound-using-class class (instance-of self) (slot-of self))))

(def method value-at-place ((self object-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-value-using-class class (instance-of self) (slot-of self))))

(def method (setf value-at-place) (new-value (self object-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (setf (slot-value-using-class class (instance-of self) (slot-of self)) new-value)))

(def (function e) make-object-slot-place (instance slot)
  (check-type instance (or structure-object standard-object))
  (check-type slot (or symbol effective-slot-definition))
  (when (symbolp slot)
    (setf slot (find-slot (class-of instance) slot)))
  (make-instance 'object-slot-place :instance instance :slot slot))

;;;;;;
;;; place-group

(def (class* e) place-group ()
  ((name :type string)
   (places :type list))
  (:documentation "A list of PLACEs with a given name."))

(def (function e) make-place-group (name places)
  (check-type name symbol)
  (check-type places sequence)
  (make-instance 'place-group :name name :places places))
