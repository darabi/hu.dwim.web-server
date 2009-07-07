;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component definer

(def (definer e :available-flags "eas") component (name supers slots &rest options)
  "Defines components with COMPONENT-CLASS metaclass."
  `(def (class* ,@-options-) ,name ,supers
     ,slots
     (:metaclass component-class)
     (:accessor-name-transformer 'dwim-accessor-name-transformer)
     ,@options))

;;;;;;
;;; Component localization

(def resource-loading-locale-loaded-listener wui-resource-loader/component :wui "resource/component/" :log-discriminator "WUI")

(register-locale-loaded-listener 'wui-resource-loader/component)

;;;;;;
;;; Component

(def (type e) component* ()
  "The generic COMPONENT type, including supported primitive COMPONENT types: NUMBER and STRING."
  '(or number string component))

(def (type e) components (&optional element-type)
  "A polimorph type for a SEQUENCE of COMPONENTs."
  (declare (ignore element-type))
  'sequence)

(def (component e) component ()
  ()
  (:documentation "
COMPONENT is the base class for all COMPONENT-CLASSes. The primitive types STRING and NUMBER are also considered COMPONENTs.
For debugging purposes NIL is not a valid COMPONENT.

Naming convention for non instantiatable components:
*/mixin     - adds some slots and/or behavior, but it is not usable on its own (usually has no superclasses, or only other mixin classes)
*/abstract  - base class for similar kind of components, usually related to a mixin that mixes in an instance of this component in a slot
              it usually has no superclasses, except other kind of mixins, and usually there is only one abstract superclass of an instantiatable component

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
;;; Component minimal

(def (component e) component/minimal (parent/mixin visibility/mixin)
  ()
  (:documentation "A COMPONENT/MINIMAL includes a minimal set of MIXINs. It supports navigation towards the ROOT-COMPONENT in the COMPONENT-HIERARCHY with PARENT-COMPONENT-OF, it also provides VISIBILITY with HIDE-COMPONENT and SHOW-COMPONENT."))

;;;;;;
;;; Component basic

(def (component e) component/basic (component/minimal refreshable/mixin renderable/mixin)
  ()
  (:documentation "A COMPONENT/BASIC includes a basic set of MIXINs. It supports reacting upon state changes with REFRESH-COMPONENT and partial rendering with RENDER-COMPONENT."))

;;;;;;
;;; Component style

(def (component e) component/style (component/basic style/abstract disableable/mixin)
  ()
  (:documentation "A COMPONENT/STYLE includes a set of style related MIXINs. It supports styles with STYLE-CLASS and CUSTOM-STYLE, it also provides REMOTE-SETUP with the help of a unique ID, and ENABLE-COMPONENT along with DISABLE-COMPONENT for better user experience."))

;;;;;;
;;; Component full

(def (component e) component/full (component/style collapsible/mixin tooltip/mixin)
  ()
  (:documentation "A COMPONENT/FULL includes all generally useful MIXINs. It supports EXPAND-COMPONENT and COLLAPSE-COMPONENT, it also provides tooltip support."))

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
  (operation-not-supported "Cannot provide PARENT-COMPONENT for ~A, you may want to subclass PARENT/MIXIN" self))

(def method (setf parent-component-of) (new-value (self component))
  (operation-not-supported "Cannot change PARENT-COMPONENT for ~A, you may want to subclass PARENT/MIXIN" self))

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

(def (function e) find-child-component (component function)
  (ensure-functionf function)
  (map-child-components component (lambda (child)
                                    (when (funcall function child)
                                      (return-from find-child-component child))))
  nil)

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
  (awhen (component-value-of self)
    (class-of it)))

(def method component-dispatch-prototype ((self component))
  (awhen (component-dispatch-class self)
    (class-prototype it)))

;;;;;;
;;; Component value

(def method component-value-of ((self component))
  nil)

(def method (setf component-value-of) (new-value (self component))
  (values))

(def method reuse-component-value ((self component) class prototype value)
  (values))

;;;;;;
;;; Component editing

(def method editable-component? ((self component))
  #f)

(def method edited-component? ((self component))
  #f)

(def method begin-editing ((self component))
  (operation-not-supported "Cannot BEGIN-EDITING under ~A, you may want to subclass EDITABLE/MIXIN" self))

(def method save-editing ((self component))
  (operation-not-supported "Cannot SAVE-EDITING under ~A, you may want to subclass EDITABLE/MIXIN" self))

(def method cancel-editing ((self component))
  (operation-not-supported "Cannot CANCEL-EDITING under ~A, you may want to subclass EDITABLE/MIXIN" self))

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
  (operation-not-supported "Cannot EXPORT-TEXT ~A, you may want to subclass EXPORTABLE/ABSTRACT"))

(def layered-method export-csv ((self component))
  (operation-not-supported "Cannot EXPORT-CSV ~A, you may want to subclass EXPORTABLE/ABSTRACT"))

(def layered-method export-pdf ((self component))
  (operation-not-supported "Cannot EXPORT-PDF ~A, you may want to subclass EXPORTABLE/ABSTRACT"))

(def layered-method export-odt ((self component))
  (operation-not-supported "Cannot EXPORT-ODT ~A, you may want to subclass EXPORTABLE/ABSTRACT"))

(def layered-method export-ods ((self component))
  (operation-not-supported "Cannot EXPORT-ODS ~A, you may want to subclass EXPORTABLE/ABSTRACT"))

;;;;;;
;;; Render component

(def render-component component
  (operation-not-supported "Cannot render ~A, you may want to override RENDER-COMPONENT" -self-))

(def render-component :around component
  (with-component-environment -self-
    (call-next-method)))

(def method to-be-rendered-component? ((self component))
  #t)

(def method mark-to-be-rendered-component ((self component))
  (map-child-components self #'mark-to-be-rendered-component))

(def method mark-rendered-component ((self component))
  (operation-not-supported "Cannot MARK-RENDERED-COMPONENT ~A, you may want to subclass RENDERABLE/MIXIN"))

;;;;;;
;;; Refresh component

(def layered-method refresh-component ((self component))
  (values))

(def method to-be-refreshed-component? ((self component))
  #f)

(def method mark-to-be-refreshed-component ((self component))
  (map-child-components self #'mark-to-be-refreshed-component))

(def method mark-refreshed-component ((self component))
  (operation-not-supported "Cannot MARK-REFRESHED-COMPONENT ~A, you may want to subclass REFRESHABLE/MIXIN"))

;;;;;;
;;; Show/hide component

(def method hideable-component? ((self component))
  #f)

(def method visible-component? ((self component))
  #t)

(def method hide-component ((self component))
  (operation-not-supported "Cannot HIDE-COMPONENT ~A, you may want to subclass VISIBILITY/MIXIN"))

(def method show-component ((self component))
  (values))

(def method hide-component-recursively ((self component))
  (map-descendant-components self #'hide-component))

(def method show-component-recursively ((self component))
  (map-descendant-components self #'show-component))

;;;;;;
;;; Enable/disable component

(def method disableable-component? ((self component))
  #f)

(def method enabled-component? ((self component))
  #t)

(def method disable-component ((self component))
  (operation-not-supported "Cannot DISABLE-COMPONENT ~A, you may want to subclass DISABLEABLE/MIXIN"))

(def method enable-component ((self component))
  (values))

(def method disable-component-recursively ((self component))
  (map-descendant-components self #'disable-component))

(def method enable-component-recursively ((self component))
  (map-descendant-components self #'enable-component))

;;;;;;
;;; Expand/collapse component

(def method collapsible-component? ((self component))
  #f)

(def method expanded-component? ((self component))
  #t)

(def method collapse-component ((self component))
  (operation-not-supported "Cannot COLLAPSE-COMPONENT ~A, you may want to subclass COLLAPSIBLE/MIXIN"))

(def method expand-component ((self component))
  (values))

(def method collapse-component-recursively ((self component))
  (map-descendant-components self #'collapse-component))

(def method expand-component-recursively ((self component))
  (map-descendant-components self #'expand-component))

;;;;;;
;;; Command

(def layered-method make-menu-bar-items ((component component) class prototype value)
  nil)

(def layered-method make-context-menu-items ((component component) class prototype value)
  nil)

(def layered-method make-command-bar-commands ((component component) class prototype value)
  nil)

;;;;;;
;;; Clone component

(def method clone-component ((self component))
  (operation-not-supported "Cannot clone ~A, you may want to subclass from CLONEABLE/ABSTRACT" self))

;;;;;;
;;; Export CSV

(def method write-csv-element ((self component))
  (render-component self))

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
                             (slot-boundp-using-class class self slot))
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
