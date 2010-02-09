;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

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

(def localization-loader-callback wui-component-localization-loader :hu.dwim.wui "localization/component/" :log-discriminator "hu.dwim.wui.component")

;;;;;;
;;; Component

(def (type e) component/immediate ()
  "Some primitive TYPEs are considered to be COMPONENTs."
  '(or number string))

(def (type e) component* ()
  "The generic COMPONENT type, including supported primitive COMPONENT types."
  '(or component/immediate component))

(def (function e) component? (object)
  (typep object 'component*))

(def (type e) components (&optional element-type)
  "A polimorph type for a SEQUENCE of COMPONENTs."
  (declare (ignore element-type))
  '(and sequence
        hash-table
        (satisfies component-sequence?)))

(def (function e) component-sequence? (sequence)
  (every #'component? sequence))

(def (component e) component ()
  ()
  (:documentation "COMPONENT is the base class for all COMPONENT-CLASSes. The primitive types STRING and NUMBER are also considered COMPONENTs.
For debugging purposes NIL is not a valid COMPONENT. This class does not have any slots on purpose.

Naming convention for non instantiatable components:
*/mixin       - adds some slots and/or behavior, but it is not usable on its own. It usually has no superclasses, or only other mixin classes.
*/abstract    - base class for similar kind of components, usually related to a mixin that mixes in an instance of this component in a slot. It usually has no superclasses, except other mixins, and usually there is only one abstract superclass of an instantiatable component.

Naming convention for presentations related to a lisp type, they are usually subclasses of alternator components:
*/maker       - subclasses of MAKER/ABSTRACT
*/viewer      - subclasses of VIEWER/ABSTRACT
*/editor      - subclasses of EDITOR/ABSTRACT
*/inspector   - subclasses of INSPECTOR/ABSTRACT
*/filter      - subclasses of FILTER/ABSTRACT
*/finder      - subclasses of FINDER/ABSTRACT
*/selector    - subclasses of SELECTOR/ABSTRACT

Naming convention for some alternative components:
*/reference/* - subclasses of reference/abstract
*/detail/*    - subclasses of detail/abstract

Components are created by either using the component specific macros, maker functions or by calling generic factory functions
such as MAKE-INSTANCE, MAKE-MAKER, MAKE-VIEWER, MAKE-EDITOR, MAKE-INSPECTOR, MAKE-FILTER, MAKE-FINDER and MAKE-SELECTOR."))

;;;;;;
;;; component/minimal

(def (component e) component/minimal (parent/mixin hideable/mixin)
  ()
  (:documentation "A COMPONENT/MINIMAL includes a minimal set of mixins. It supports navigation towards the root component in the component hierarchy using PARENT-COMPONENT-OF, it also provides HIDE-COMPONENT and SHOW-COMPONENT."))

;;;;;;
;;; component/basic

(def (component e) component/basic (component/minimal refreshable/mixin renderable/mixin)
  ()
  (:documentation "A COMPONENT/BASIC includes a basic set of mixins. It supports reacting upon state changes with TO-BE-REFRESHED-COMPONENT? and REFRESH-COMPONENT. It also provides on demand rendering with TO-BE-RENDERED-COMPONENT? and RENDER-COMPONENT."))

;;;;;;
;;; component/style

(def (component e) component/style (component/basic style/abstract disableable/mixin)
  ()
  (:documentation "A COMPONENT/STYLE includes a set of style related mixins. It supports styles with STYLE-CLASS and CUSTOM-STYLE, it also provides remote setup with the help of a unique ID, and ENABLE-COMPONENT along with DISABLE-COMPONENT for better user experience."))

;;;;;;
;;; component/full

(def (component e) component/full (component/style collapsible/mixin tooltip/mixin)
  ()
  (:documentation "A COMPONENT/FULL includes all generally useful mixins and might be extended later. Currently it supports EXPAND-COMPONENT and COLLAPSE-COMPONENT, it also provides tooltips."))

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
      (place-value place)
    (assert (typep value '(or null component)))))

(def method (setf component-at-place) ((replacement-component component) (place place))
  (when-bind replacement-place (make-component-place replacement-component)
    (setf (place-value replacement-place) nil))
  (when-bind original-component (place-value place)
    (when (typep original-component 'parent/mixin)
      (setf (parent-component-of original-component) nil)))
  (setf (place-value place) replacement-component))

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

(def (function e) map-child-components (component visitor &optional (child-slot-provider [class-slots (class-of !1)]))
  (ensure-functionf visitor)
  (iter (with class = (class-of component))
        (for slot :in (funcall child-slot-provider component))
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
;;; Component documentation

(def methods component-documentation
  (:method ((self symbol))
    (component-documentation (find-class self)))

  (:method ((self component-class))
    (component-documentation (class-prototype self)))

  (:method ((self component))
    (or (documentation (class-of self) 'type)
        "No documentation")))

;;;;;;
;;; Component style

(def method component-style-class ((self component))
  (string-downcase (substitute #\Space #\/ (symbol-name (class-name (class-of self))))))

;;;;;;
;;; Component value

(def method component-value-of ((self component))
  nil)

(def method (setf component-value-of) (new-value (self component))
  (values))

(def method reuse-component-value ((self component) class prototype value)
  value)

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

(def function export-operation-not-supported (thing export-method-name)
  (operation-not-supported "Cannot export ~A; you may want to specialize the ~S method on it." thing export-method-name))

(def layered-method export-text ((self component))
  (export-operation-not-supported self 'export-text))

(def layered-method export-csv ((self component))
  (export-operation-not-supported self 'export-csv))

(def layered-method export-pdf ((self component))
  (export-operation-not-supported self 'export-pdf))

(def layered-method export-odt ((self component))
  (export-operation-not-supported self 'export-odt))

(def layered-method export-ods ((self component))
  (export-operation-not-supported self 'export-ods))

;;;;;;
;;; Render component

(def render-component component
  (operation-not-supported "Cannot render ~A, you may want to override RENDER-COMPONENT" -self-))

(def render-component :around component
  (with-component-environment -self-
    (call-next-method)))

(def method to-be-rendered-component? ((self component))
  #f)

(def method mark-to-be-rendered-component ((self component))
  (map-child-components self #'mark-to-be-rendered-component))

(def method mark-rendered-component ((self component))
  (operation-not-supported "Cannot MARK-RENDERED-COMPONENT ~A, you may want to subclass RENDERABLE/MIXIN"))

;;;;;;
;;; Refresh component

(def layered-method refresh-component ((self component))
  (values))

;; TODO: this supposed to happen at the very end, but now it's at the very beginning of :after methods
(def layered-method refresh-component :after ((self component))
  (bind ((class (class-of self)))
    (dolist (slot (computed-slots-of class))
      (when (slot-boundp-using-class class self slot)
        (slot-value-using-class class self slot)))))

(def method to-be-refreshed-component? ((self component))
  #f)

(def method to-be-refreshed-component? :around ((self component))
  (or (call-next-method)
      (some (lambda (slot)
              (not (computed-slot-valid-p self slot)))
            (computed-slots-of (class-of self)))))

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
  (operation-not-supported "Cannot HIDE-COMPONENT ~A, you may want to subclass HIDEABLE/MIXIN"))

(def method show-component ((self component))
  (values))

(def method hide-component-recursively ((self component))
  (map-descendant-components self #'hide-component))

(def method show-component-recursively ((self component))
  (map-descendant-components self #'show-component))

;;;;;;
;;; Traverse visible components

(def (generic e) visible-child-component-slots (component)
  (:method ((self component))
    (class-slots (class-of self))))

(def (generic e) map-visible-child-components (component function)
  (:method ((component component) function)
    (ensure-functionf function)
    (map-child-components component (lambda (child)
                                      (when (visible-component? child)
                                        (funcall function child)))
                          #'visible-child-component-slots)))

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

(def layered-method make-move-commands ((component component) class prototype value)
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
  (declare (optimize debug))
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
                               *print-length*
                               (> (length value) *print-length*))
                          (progn
                            (prin1 (subseq value 0 *print-length*))
                            (bind ((*print-circle* #f))
                              (princ "...>")))
                          (prin1 value))))))))))
