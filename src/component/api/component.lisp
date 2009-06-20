;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component definer

(def (definer e :available-flags "eas") component (name supers slots &rest options)
  `(def (class* ,@-options-) ,name ,supers
     ,slots
     (:metaclass component-class)
     (:accessor-name-transformer 'dwim-accessor-name-transformer)
     ,@options))

;;;;;;
;;; Component localization

(def resource-loading-locale-loaded-listener wui-resource-loader/component :wui "resource/component/"
  :log-discriminator "WUI")

(register-locale-loaded-listener 'wui-resource-loader/component)

;;;;;;
;;; Component base class

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

;;;;;;
;;; Component environment

(def (macro e) with-component-environment (component &body forms)
  `(call-in-component-environment ,component (named-lambda with-component-environment-body ()
                                               ,@forms)))

(def (with-macro e) with-restored-component-environment (component)
  (bind ((path (nreverse (collect-path-to-root-component component))))
    (labels ((%call-with-restored-component-environment (remaining-path)
               (if remaining-path
                   (with-component-environment (car remaining-path)
                     (%call-with-restored-component-environment (cdr remaining-path)))
                   (-body-))))
      (%call-with-restored-component-environment path))))

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

;;;;;;
;;; Component computed slot

(def method call-compute-as ((self component) thunk)
  (funcall thunk))

;;;;;;
;;; Parent component

(def method parent-component-of ((self component))
  (operation-not-supported))

(def method (setf parent-component-of) (new-value (self component))
  (operation-not-supported))

(def method child-component-slot? ((self component) (slot standard-effective-slot-definition))
  #f)

(def method child-component-slot? ((self component) (slot component-effective-slot-definition))
  #t)

;;;;;;
;;; Parent child relationship

(def (function e) find-ancestor-component (component predicate)
  (find-ancestor component #'parent-component-of predicate))

(def (function e) find-ancestor-component-with-type (component type)
  (find-ancestor-component component [typep !1 type]))

(def (function e) map-ancestor-components (component visitor &key (include-self #f))
  (ensure-functionf visitor)
  (labels ((traverse (current)
             (awhen (parent-component-of current)
               (funcall visitor it)
               (traverse it))))
    (when include-self
      (funcall visitor component))
    (traverse component)))

(def (function e) collect-path-to-root-component (component)
  (iter (for ancestor-component :initially component :then (parent-component-of ancestor-component))
        (while ancestor-component)
        (collect ancestor-component)))

(def (function e) collect-ancestor-components (component &key (include-self #f))
  (nconc (when include-self
           (list component))
         (iter (for parent :first component :then (parent-component-of parent))
               (while parent)
               (collect parent))))

(def (function e) find-descendant-component (component predicate)
  (map-descendant-components component (lambda (child)
                                         (when (funcall predicate child)
                                           (return-from find-descendant-component child)))))

(def (function e) find-descendant-component-with-type (component type)
  (find-descendant-component component [typep !1 type]))

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

;;;;;;
;;; Component environment

(def method call-in-component-environment ((self component) thunk)
  (funcall thunk))

;;;;;;
;;; Component dispatch class/prototype

(def method component-dispatch-class ((self component))
  nil)

(def method component-dispatch-prototype ((self component))
  (awhen (component-dispatch-class self)
    (class-prototype it)))

;;;;;;
;;; Component value

(def method component-value-of ((self component))
  nil)

(def method (setf component-value-of) (new-value (self component))
  (operation-not-supported))

(def method reuse-component-value ((self component) class prototype value)
  (values))

;;;;;;
;;; Component editing

(def method editable-component? ((self component))
  #f)

(def method edited-component? ((self component))
  (operation-not-supported))

(def method begin-editing ((self component))
  (operation-not-supported))

(def method save-editing ((self component))
  (operation-not-supported))

(def method cancel-editing ((self component))
  (operation-not-supported))

(def methods store-editing
  (:method :around ((self component))
    (call-in-component-environment self #'call-next-method))

  (:method ((self component))
    (map-editable-child-components self #'store-editing)))

(def methods revert-editing
  (:method :around ((self component))
    (call-in-component-environment self #'call-next-method))

  (:method ((self component))
    (map-editable-child-components self #'revert-editing)))

(def methods join-editing
  (:method :around ((self component))
    (call-in-component-environment self #'call-next-method))

  (:method ((self component))
    (map-editable-child-components self #'join-editing)))

(def methods leave-editing
  (:method :around ((self component))
    (call-in-component-environment self #'call-next-method))

  (:method ((self component))
    (map-editable-child-components self #'leave-editing)))

;;;;;;
;;; Traverse editable components

(def (function e) map-editable-child-components (component function)
  (ensure-functionf function)
  (map-child-components component (lambda (child)
                                    (when (editable-component? child)
                                      (funcall function child)))))

(def (function e) map-editable-descendant-components (component function)
  (ensure-functionf function)
  (map-editable-child-components component (lambda (child)
                                             (funcall function child)
                                             (map-editable-descendant-components child function))))

(def (function e) find-editable-child-component (component function)
  (ensure-functionf function)
  (map-editable-child-components component (lambda (child)
                                             (when (funcall function child)
                                               (return-from find-editable-child-component child))))
  nil)

(def (function e) find-editable-descendant-component (component function)
  (map-editable-descendant-components component (lambda (descendant)
                                                  (when (funcall function descendant)
                                                    (return-from find-editable-descendant-component descendant))))
  nil)

(def (function e) has-edited-child-component-p (component)
  (find-editable-child-component component #'edited-component?))

(def (function e) has-edited-descendant-component-p (component)
  (find-editable-descendant-component component #'edited-component?))

;;;;;;
;;; Export component

(def layered-method export-text ((self component))
  (operation-not-supported))

(def layered-method export-csv ((self component))
  (operation-not-supported))

(def layered-method export-pdf ((self component))
  (operation-not-supported))

(def layered-method export-odt ((self component))
  (operation-not-supported))

(def layered-method export-ods ((self component))
  (operation-not-supported))

;;;;;;
;;; Render component

(def render-component component
  (operation-not-supported))

(def method to-be-rendered-component? (component)
  #t)

(def method mark-to-be-rendered-component (component)
  (operation-not-supported))

(def method mark-rendered-component (component)
  (operation-not-supported))

;;;;;;
;;; Refresh component

(def layered-method refresh-component ((self component))
  (values))

(def method to-be-refreshed-component? ((self component))
  #f)

(def method mark-to-be-refreshed-component ((self component))
  (operation-not-supported))

(def method mark-refreshed-component ((self component))
  (operation-not-supported))

;;;;;;
;;; Show/hide component

(def method visible-component? ((self component))
  #t)

(def method hide-component ((self component))
  (operation-not-supported))

(def method show-component ((self component))
  (values))

(def method hide-component-recursively ((self component))
  (map-descendant-components self #'hide-component))

(def method show-component-recursively ((self component))
  (map-descendant-components self #'show-component))

;;;;;;
;;; Enable/disable component

(def method enabled-component? ((self component))
  #t)

(def method disable-component ((self component))
  (operation-not-supported))

(def method enable-component ((self component))
  (values))

(def method disable-component-recursively ((self component))
  (map-descendant-components self #'disable-component))

(def method enable-component-recursively ((self component))
  (map-descendant-components self #'enable-component))

;;;;;;
;;; Expand/collapse component

(def method expanded-component? ((self component))
  #t)

(def method collapse-component ((self component))
  (operation-not-supported))

(def method expand-component ((self component))
  (values))

(def method collapse-component-recursively ((self component))
  (map-descendant-components self #'collapse-component))

(def method expand-component-recursively ((self component))
  (map-descendant-components self #'expand-component))

;;;;;;
;;; Clone component

(def method clone-component ((self component))
  (operation-not-supported))

;;;;;;
;;; Export CSV

(def method write-csv-element ((self component))
  (render-component self))

;;;;;;
;;; Component layout

(def (component e) component/layout (parent/mixin visibility/mixin)
  ()
  (:documentation "A LAYOUT is a COMPONENT which does not have any visual appearance on its own. If all child COMPONENTS positioned by a LAYOUT is EMPTY then the whole COMPONENT is invisible."))

;;;;;;
;;; Component basic

(def (component e) component/basic (renderable/mixin refreshable/mixin parent/mixin visibility/mixin)
  ()
  (:documentation "A base class for COMPONENTs."))

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
                          (prin1 value))))))))))
