;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place

(def class* place ()
  ())

(def (generic e) place-type (place))

(def (generic e) place-bound-p (place))

(def (generic e) place-editable-p (place))

(def (generic e) make-place-unbound (place))

(def (generic e) value-at-place (place))

(def (generic e) (setf value-at-place) (new-value place))

;;;;;;
;;; Special variable place

(def class* special-variable-place (place)
  ((name)
   (the-type)))

(def method place-type ((place special-variable-place))
  (the-type-of place))

(def method place-bound-p ((place special-variable-place))
  (boundp (name-of place)))

(def method place-editable-p ((place special-variable-place))
  #t)

(def method make-place-unbound ((place special-variable-place))
  (makunbound (name-of place)))

(def method value-at-place ((place special-variable-place))
  (symbol-value (name-of place)))

(def method (setf value-at-place) (new-value (place special-variable-place))
  (setf (symbol-value (name-of place)) new-value))

(def (function e) make-special-variable-place (name type)
  (make-instance 'special-variable-place :name name :the-type type))

;;;;;;
;;; Lexical variable place

(def class* lexical-variable-place (place)
  ((getter)
   (setter)
   (the-type)))

(def method place-type ((place lexical-variable-place))
  (the-type-of place))

(def method place-bound-p ((place lexical-variable-place))
  #t)

(def method place-editable-p ((place lexical-variable-place))
  #t)

(def method make-place-unbound ((place lexical-variable-place))
  (error "Cannot make ~A unbound" place))

(def method value-at-place ((place lexical-variable-place))
  (funcall (getter-of place)))

(def method (setf value-at-place) (new-value (place lexical-variable-place))
  (funcall (setter-of place) new-value))

(def (macro e) make-lexical-variable-place (name type)
  `(make-instance 'lexical-variable-place
                  :getter (lambda ()
                            ,name)
                  :setter (lambda (value)
                            (setf ,name value))
                  :the-type ,type))

;;;;;;
;;; Slot value place

(def class* slot-value-place (place)
  ((instance)
   (slot)))

(def function revive-slot-value-place-instance (place)
  (bind ((instance (instance-of place)))
    (if (and (typep instance 'prc::persistent-object)
             (prc::persistent-p instance))
        (prc::revive-instance (instance-of place))
        instance)))

(def generic slot-type (slot)
  (:method ((slot standard-slot-definition))
    (slot-definition-type slot))

  (:method ((slot prc::persistent-slot-definition))
    (prc::canonical-type-of slot)))

(def method place-type ((place slot-value-place))
  (slot-type (slot-of place)))

(def method place-bound-p ((place slot-value-place))
  (bind ((instance (revive-slot-value-place-instance place)))
    (slot-boundp-using-class (class-of instance) instance (slot-of place))))

(def (generic e) slot-value-place-editable-p (place class instance slot)
  (:method ((place slot-value-place) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
    #t)

  (:method ((place slot-value-place) (class cc::computed-class) (instance cc::computed-object) (slot cc::computed-effective-slot-definition))
    #f))

(def method place-editable-p ((place slot-value-place))
  (bind ((instance (instance-of place))
         (class (class-of instance)))
    (slot-value-place-editable-p place class instance (slot-of place))))

(def method make-place-unbound ((place slot-value-place))
  (bind ((instance (revive-slot-value-place-instance place)))
    (slot-makunbound-using-class (class-of instance) instance (slot-of place))))

(def method value-at-place ((place slot-value-place))
  (bind ((instance (revive-slot-value-place-instance place)))
    (slot-value-using-class (class-of instance) instance (slot-of place))))

(def method (setf value-at-place) (new-value (place slot-value-place))
  (bind ((instance (revive-slot-value-place-instance place)))
    (setf (slot-value-using-class (class-of instance) instance (slot-of place)) new-value)))

(def (function e) make-slot-value-place (instance slot)
  (make-instance 'slot-value-place :instance instance :slot slot))

;;;;;;
;;; List slot value place

(def class* list-slot-value-place (slot-value-place)
  ((index)))

(def method place-bound-p ((place list-slot-value-place))
  #t)

(def method make-place-unbound ((place list-slot-value-place))
  (error "Cannot make ~A unbound" place))

(def method value-at-place ((place list-slot-value-place))
  (nth (index-of place) (call-next-method)))

(def method (setf value-at-place) (new-value (place list-slot-value-place))
  (bind ((instance (instance-of place))
         (slot (slot-of place))
         (class (class-of instance)))
    (bind ((list (slot-value-using-class class instance slot)))
      (setf (nth (index-of place) list) new-value)
      (setf (slot-value-using-class class instance slot) list))))

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
