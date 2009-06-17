;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Definer

(def (definer e :available-flags "eas") component (name supers slots &rest options)
  `(def (class* ,@-options-) ,name ,supers
     ,slots
     (:metaclass component-class)
     (:accessor-name-transformer 'dwim-accessor-name-transformer)
     ,@options))

;;;;;;
;;; Component

(def (type e) component* ()
  "The generic component type, including supported primitive component types."
  '(or number string component))

(def (type e) components (&optional element-type)
  "A polimorph type for a sequence of components."
  (declare (ignore element-type))
  'sequence)

(def (component e) component ()
  ()
  (:documentation "
Component is the base class for all components, except for primitive strings and numbers that are also considered components.
For debugging purposes NIL is not a component.

Naming convention for non instantiatable components:
*/mixin     - adds some slots and/or behavior, but it is not usable on its own (usually has no superclasses, or only other mixin classes)
*/abstract  - base class for similar kind of components, usually related to a mixin that mixes in an instance of this component in a slot
              it usually has no superclasses, except other kind of mixins, and usually there is only one abstract superclass of an instantiatable component

Naming convention for standard instantiatable components:
*/basic     - subclass of the minimal set of mixins to have a usable component
*/full      - subclass of the maximal set of mixins to have a full featured component

Naming convention for meta components related to a lisp type, they are usually alternator components for compound types:
*/maker     - subclasses of maker/abstract
*/viewer    - subclasses of viewer/abstract
*/editor    - subclasses of editor/abstract
*/inspector - subclasses of inspector/abstract
*/filter    - subclasses of filter/abstract
*/finder    - subclasses of finder/abstract
*/selector  - subclasses of selector/abstract

Naming convention for some alternative components:
*/brief/*   - subclasses of brief/abstract
*/detail/*  - subclasses of detail/abstract

Components are created by either using the component specific factory macros, makers or by calling generic factory methods
such as make-instance, make-maker, make-viewer, make-editor, make-inspector, make-filter, make-finder and make-selector.
"))

(def layered-method call-in-component-environment ((self component) thunk)
  (funcall thunk))

(def method component-dispatch-class ((self component))
  nil)

(def method component-dispatch-prototype ((self component))
  (awhen (component-dispatch-class component)
    (class-prototype it)))

(def method supports-debug-component-hierarchy? ((self component))
  #t)

(def method visible? ((self component))
  #t)

(def layered-method clone-component ((self component))
  (make-instance (class-of self)))

(def method child-component-slot? ((self component) (slot standard-effective-slot-definition))
  #f)

(def method child-component-slot? ((self component) (slot component-effective-slot-definition))
  #t)

(def method write-csv-element ((self component))
  (render-component self))

;;;;;;
;;; Number

(def (special-variable e) *text-stream* "The output stream for rendering components in text format.")

(def render-xhtml number
  `xml,-self-)

(def render-text number
  (write -self- :stream *text-stream*))

(def render-csv number
  (write-csv-value (princ-to-string -self-)))

(def render-ods number
  <text:p ,-self->)

(def render-odt number
  <text:p ,-self->)

(def layered-method refresh-component ((self number))
  (values))

(def method component-dispatch-class ((self number))
  (find-class 'number))

(def method component-dispatch-prototype ((self number))
  (class-prototype (find-class 'number)))

(def method mark-to-be-refreshed ((self number))
  (values))

(def method mark-refreshed ((self number))
  (values))

(def method visible? ((self number))
  #t)

(def layered-method clone-component ((self number))
  self)

;;;;;;
;;; String

(def render-xhtml string
  `xml,-self-)

(def render-text string
  (write -self- :stream *text-stream*))

(def render-csv string
  (write-csv-value -self-))

(def render-ods string
  <text:p ,-self->)

(def render-odt string
  <text:p ,-self->)

(def layered-method refresh-component ((self string))
  (values))

(def method component-dispatch-class ((self string))
  (find-class 'string))

(def method component-dispatch-prototype ((self string))
  (class-prototype (find-class 'string)))

(def method mark-to-be-refreshed ((self string))
  (values))

(def method mark-refreshed ((self string))
  (values))

(def method visible? ((self string))
  #t)

(def layered-method clone-component ((self string))
  (copy-seq self))

;;;;;;
;;; Component basic

(def (component ea) component/basic (renderable/mixin refreshable/mixin parent/mixin visible/mixin)
  ())

;;;;;;
;;; Component full

(def (component ea) component/full (component/basic style/abstract enabled/mixin)
  ())

;;;;;;
;;; Print component

(def method print-object ((self component) *standard-output*)
  (print-component self))

(def method print-component ((self component) &optional (*standard-output* *standard-output*))
  (if (and *print-level*
           (>= *component-print-level* *print-level*))
      (write-char #\#)
      (bind ((*component-print-level* (1+ *component-print-level*)))
        (handler-bind ((error (lambda (error)
                                (declare (ignore error))
                                (write-string "<<error printing component>>")
                                (return-from print-component))))
          (pprint-logical-block (*standard-output* nil :prefix "#<" :suffix ">")
            (pprint-indent :current 1)
            (bind ((*print-circle* #f))
              (princ (symbol-name (class-name (class-of self)))))
            (iter (with class = (class-of self))
                  (repeat (or *print-length* most-positive-fixnum))
                  (for slot :in (class-slots class))
                  (when (and (child-component-slot? self slot)
                             (slot-value-using-class class self slot))
                    (bind ((initarg (first (slot-definition-initargs slot)))
                           (value (slot-value-using-class class self slot)))
                      (write-char #\Space)
                      (pprint-newline :fill)
                      (prin1 initarg)
                      (write-char #\Space)
                      (pprint-newline :fill)
                      (if (and (stringp value)
                               (> (length value) *print-length*))
                          (progn
                            (prin1 (subseq value 0 *print-length*))
                            (bind ((*print-circle* #f))
                              (princ "...>")))
                          (princ value))))))))))
