;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; abstract-place

(def (class* e) abstract-place ()
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

(def (class* e) basic-place (abstract-place)
  ((name :type symbol)
   (initform :type t)
   (the-type :type t))
  (:documentation "An abstract basic PLACE with a few properties."))

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
  ((argument :type t)
   (getter :type function)
   (setter :type function))
  (:documentation "A PLACE that is get and set by using lambda functions called with its argument."))

(def method value-at-place ((self functional-place))
  (funcall (getter-of self) (argument-of self)))

(def method (setf value-at-place) (new-value (self functional-place))
  (funcall (setter-of self) new-value (argument-of self)))

(def (function e) make-functional-place (argument name)
  (make-instance 'functional-place
                 :argument argument
                 :name name
                 :getter (fdefinition name)
                 :setter (fdefinition `(setf ,name))))

;;;;;;
;;; variable-place

(def (class*) variable-place (basic-place)
  ())

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

(def (function e) make-special-variable-place (name type)
  (make-instance 'special-variable-place :name name :the-type type))

;;;;;;
;;; lexical-variable-place

(def class* lexical-variable-place (variable-place)
  ((getter :type function)
   (setter :type function))
  (:documentation "A PLACE for a lexical variable."))

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
;;; sequence-place

(def class* sequence-place (basic-place)
  ((sequence :type sequence)
   (index :type integer)))

(def method value-at-place ((self basic-place))
  (elt (sequence-of self) (index-of self)))

(def method (setf value-at-place) (new-value (self basic-place))
  (setf (elt (sequence-of self) (index-of self)) new-value))

(def (function e) make-sequence-place (sequence index)
  (make-instance 'sequence-place
                 :sequence sequence
                 :index index))

;;;;;;
;;; instance-slot-place

(def class* instance-slot-place (abstract-place)
  ((instance :type standard-object)
   (slot :type standard-effective-slot-definition))
  (:documentation "A PLACE for a particular slot of a STANDARD-OBJECT instance."))

(def (generic e) instance-slot-place-editable? (place class instance slot)
  (:documentation "TRUE means the PLACE can be edited and set to other values, otherwise FALSE.")

  (:method  ((place instance-slot-place) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
    #t))

(def generic slot-type (slot)
  (:documentation "Returns a lisp type specifier for SLOT.")

  (:method ((slot slot-definition))
    (slot-definition-type slot))

  #+sbcl
  (:method ((slot sb-pcl::structure-slot-definition))
    (slot-definition-type slot)))

(def method place-name ((self instance-slot-place))
  (slot-definition-name (slot-of self)))

(def method place-documentation ((self instance-slot-place))
  (documentation (slot-of self) t))

(def method place-type ((self instance-slot-place))
  (slot-type (slot-of self)))

(def method place-editable? ((self instance-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (instance-slot-place-editable? self class instance (slot-of self))))

(def method place-bound? ((self instance-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-boundp-using-class class (instance-of self) (slot-of self))))

(def method make-place-unbound ((self instance-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-makunbound-using-class class (instance-of self) (slot-of self))))

(def method value-at-place ((self instance-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (slot-value-using-class class (instance-of self) (slot-of self))))

(def method (setf value-at-place) (new-value (self instance-slot-place))
  (bind ((instance (instance-of self))
         (class (class-of instance)))
    (setf (slot-value-using-class class (instance-of self) (slot-of self)) new-value)))

(def (function e) make-instance-slot-place (instance slot)
  (when (symbolp slot)
    (setf slot (find-slot (class-of instance) slot)))
  (make-instance 'instance-slot-place :instance instance :slot slot))

;;;;;;
;;; place-group

(def class* place-group ()
  ((name :type string)
   (places :type list))
  (:documentation "A list of PLACEs with a given name."))

(def (function e) make-place-group (name places)
  (make-instance 'place-group :name name :places places))
