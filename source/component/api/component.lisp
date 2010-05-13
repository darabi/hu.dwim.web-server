;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; TODO map-* should have the visitor at first position?

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
  '(or (and sequence
            (satisfies component-sequence?))
       (and hash-table
            (satisfies component-hash-table?))))

(def (function e) component-sequence? (sequence)
  (every #'component? sequence))

(def (function e) component-hash-table? (hash-table)
  (maphash-values #'component? hash-table))

(def (component e) component ()
  ()
  (:documentation "The base class for all components. The primitive types /CLASS/COMMON-LISP:STRING and /CLASS/COMMON-LISP:NUMBER are also considered components. For debugging purposes and to avoid silent errors NIL is not a valid component. This class does not have any slots on purpose.

Naming convention for non instantiatable components:
<name>/mixin     - adds some slots and/or behavior, but it is not usable on its own. It usually has no superclasses, or only other mixin classes.
<name>/component - abstract base class for similar kind of components, usually related to a mixin that adds an instance of this component in a slot. It usually has no superclasses, except other mixins, and usually there is only one abstract superclass of an instantiatable component.

Naming convention for alternative presentations related to a lisp type, they are usually subclasses of the following components:
<lisp-type>/<alternative>/maker     - subclasses of /CLASS/'HU.DWIM.WUI:COMPONENT/MAKER'
<lisp-type>/<alternative>/viewer    - subclasses of /CLASS/'HU.DWIM.WUI:COMPONENT/VIEWER'
<lisp-type>/<alternative>/editor    - subclasses of /CLASS/'HU.DWIM.WUI:COMPONENT/EDITOR'
<lisp-type>/<alternative>/inspector - subclasses of /CLASS/'HU.DWIM.WUI:COMPONENT/INSPECTOR'
<lisp-type>/<alternative>/filter    - subclasses of /CLASS/'HU.DWIM.WUI:COMPONENT/FILTER'
<lisp-type>/<alternative>/finder    - subclasses of /CLASS/'HU.DWIM.WUI:COMPONENT/FINDER'
<lisp-type>/<alternative>/selector  - subclasses of /CLASS/'HU.DWIM.WUI:COMPONENT/SELECTOR'

Naming convention for some alternative components:
<lisp-type>/reference/<presentation-kind> - subclasses of /CLASS/'HU.DWIM.WUI:T/REFERENCE/PRESENTATION'
<lisp-type>/detail/<presentation-kind>    - subclasses of /CLASS/'HU.DWIM.WUI:T/DETAIL/PRESENTATION'

Components are created by either using the component specific macros, one of the component specific factory functions or by calling generic factory functions such as /FUNCTION/COMMON-LISP:MAKE-INSTANCE, /FUNCTION/HU.DWIM.WUI:MAKE-MAKER, /FUNCTION/HU.DWIM.WUI:MAKE-VIEWER, /FUNCTION/HU.DWIM.WUI:MAKE-EDITOR, /FUNCTION/HU.DWIM.WUI:MAKE-INSPECTOR, /FUNCTION/HU.DWIM.WUI:MAKE-FILTER, /FUNCTION/HU.DWIM.WUI:MAKE-FINDER and /FUNCTION/HU.DWIM.WUI:MAKE-SELECTOR."))

;;;;;;
;;; standard/component

(def (component e) standard/component (style/component parent/mixin hideable/mixin refreshable/mixin renderable/mixin disableable/mixin)
  ()
  (:documentation "A standard component includes a set of generally useful mixins."))

;;;;;;
;;; Component environment

(def (macro e) with-component-environment (component &body forms)
  `(call-in-component-environment ,component (named-lambda with-component-environment-body ()
                                               ,@forms)))

(def (with-macro e) with-restored-component-environment (component)
  (bind ((path (nreverse (collect-ancestor-components component))))
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

(def method (setf component-at-place) :after ((replacement-component component) (place object-slot-deep-place))
  (bind ((parent-component (instance-of place)))
    (setf (parent-component-references replacement-component) parent-component)
    (mark-to-be-rendered-component parent-component)))

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
;;; Ancestors

(def (function eo) find-ancestor-component-if (predicate component &key (otherwise :error otherwise?))
  (or (find-ancestor component #'parent-component-of predicate :otherwise #f)
      (handle-otherwise (error "~S failed; predicate ~A, starting from ~A" 'find-ancestor-component-if predicate component))))

(def (function eo) find-ancestor-component (item component &key test key (otherwise :error otherwise?))
  (bind ((test (if test (ensure-function test) #'eql))
         (key (if key (ensure-function key) #'identity)))
    (or (find-ancestor-component-if (lambda (child)
                                      (funcall test item (funcall key child)))
                                    component :otherwise #f)
        (handle-otherwise (error "~S failed; item ~A, starting from ~A, key ~A" 'find-ancestor-component item component key)))))

(def (function eio) find-ancestor-component-of-type (type component &key (otherwise :error otherwise?))
  (or (find-ancestor-component-if (of-type type) component :otherwise #f)
      (handle-otherwise (error "~S failed; type ~S, starting from ~A" 'find-ancestor-component-of-type type component))))

(def (function eo) map-ancestor-components (component visitor &key (include-self #f))
  (bind ((visitor (ensure-function visitor)))
    (labels ((traverse (current)
               (awhen (parent-component-of current)
                 (funcall visitor it)
                 (traverse it))))
      (when include-self
        (funcall visitor component))
      (traverse component))))

(def (function eo) collect-ancestor-components (component &key (include-self #f))
  (nconc (when include-self
           (list component))
         (iter (for parent :first component :then (parent-component-of parent))
               (while parent)
               (collect parent))))

;;;;;;
;;; Children

(def (function eo) find-child-component-if (predicate component &key (otherwise :error otherwise?))
  (bind ((predicate (ensure-function predicate)))
    (map-child-components component (lambda (child)
                                      (when (funcall predicate child)
                                        (return-from find-child-component-if child)))))
  (handle-otherwise (error "~S failed; predicate ~A, starting from ~A" 'find-child-component-if predicate component)))

(def (function eo) find-child-component (item component &key test key (otherwise :error otherwise?))
  (bind ((test (if test (ensure-function test) #'eql))
         (key (if key (ensure-function key) #'identity)))
    (or (find-child-component-if (lambda (child)
                                   (funcall test item (funcall key child)))
                                 component :otherwise #f)
        (handle-otherwise (error "~S failed; item ~A, starting from ~A, key ~A" 'find-child-component item component key)))))

(def (function eo) find-descendant-component-if (predicate component &key (otherwise :error otherwise?))
  (bind ((predicate (ensure-function predicate)))
    (map-descendant-components component
                               (lambda (child)
                                 (when (funcall predicate child)
                                   (return-from find-descendant-component-if child)))))
  (handle-otherwise (error "~S failed; predicate ~A, starting from ~A" 'find-descendant-component-if predicate component)))

(def (function eo) find-descendant-component (item component &key test key (otherwise :error otherwise?))
  (bind ((test (if test (ensure-function test) #'eql))
         (key (if key (ensure-function key) #'identity)))
    (or (find-descendant-component-if (lambda (child)
                                        (funcall test item (funcall key child)))
                                      component :otherwise #f)
        (handle-otherwise (error "~S failed; item ~A, starting from ~A, key ~A" 'find-descendant-component item component key)))))

(def (function eio) find-descendant-component-of-type (type component &key (otherwise :error otherwise?))
  (or (find-descendant-component-if (of-type type) component :otherwise #f)
      (handle-otherwise (error "~S failed; type ~S, starting from ~A" 'find-descendant-component-of-type type component))))

(def (function eo) map-child-components (component visitor &optional (child-slot-provider [class-slots (class-of !1)]))
  (bind ((visitor (ensure-function visitor))
         (child-slot-provider (ensure-function child-slot-provider)))
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
                 (iter (for (nil element) :in-hashtable value)
                       (when (typep element 'component)
                         (funcall visitor element))))))))))

(def (function eo) map-descendant-components (component visitor &key (include-self #f))
  (bind ((visitor (ensure-function visitor)))
    (labels ((traverse (parent-component)
               (map-child-components parent-component
                                     (lambda (child-component)
                                       (funcall visitor child-component)
                                       (traverse child-component)))))
      (when include-self
        (funcall visitor component))
      (traverse component))))

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

(def function %component-style-class (component)
  (string-downcase (substitute #\Space #\/ (symbol-name (class-name (class-of component))))))

(def method component-style-class ((self component))
  (%component-style-class self))

;;;;;;
;;; Component value

(def method component-value-of ((self component))
  nil)

(def method (setf component-value-of) (new-value (self component))
  (values))

(def method component-value= (value-1 value-2)
  (eq value-1 value-2))

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
    (map-edited-child-components self #'store-editing)))

(def methods revert-editing
  (:method :around ((self component))
    (call-in-component-environment self #'call-next-method))

  (:method ((self component))
    (map-edited-child-components self #'revert-editing)))

(def methods join-editing
  (:method :around ((self component))
    (call-in-component-environment self #'call-next-method))

  (:method ((self component))
    (map-editable-child-components self #'join-editing)))

(def methods leave-editing
  (:method :around ((self component))
    (call-in-component-environment self #'call-next-method))

  (:method ((self component))
    (map-edited-child-components self #'leave-editing)))

;;;;;;
;;; Traverse editable components

(def (function eo) map-editable-child-components (component visitor)
  (bind ((visitor (ensure-function visitor)))
    (map-child-components component (lambda (child)
                                      (when (editable-component? child)
                                        (funcall visitor child))))))

(def (function eo) map-editable-descendant-components (component visitor)
  (bind ((visitor (ensure-function visitor)))
    (map-editable-child-components component (lambda (child)
                                               (funcall visitor child)
                                               (map-editable-descendant-components child visitor)))))

(def (function eo) find-editable-child-component-if (predicate component &key (otherwise :error otherwise?))
  (bind ((predicate (ensure-function predicate)))
    (map-editable-child-components component (lambda (child)
                                               (when (funcall predicate child)
                                                 (return-from find-editable-child-component-if child)))))
  (handle-otherwise (error "~S failed using preficate ~A, starting from ~A" 'find-editable-child-component-if predicate component)))

(def (function eo) find-editable-descendant-component-if (predicate component &key (otherwise :error otherwise?))
  (bind ((predicate (ensure-function predicate)))
    (map-editable-descendant-components component (lambda (descendant)
                                                    (when (funcall predicate descendant)
                                                      (return-from find-editable-descendant-component-if descendant)))))
  (handle-otherwise (error "~S failed; predicate ~A, starting from ~A" 'find-editable-descendant-component-if predicate component)))

(def (function e) has-editable-child-component? (component)
  (find-editable-child-component-if #'editable-component? component :otherwise #f))

(def (function e) has-editable-descendant-component? (component)
  (find-editable-descendant-component-if #'editable-component? component :otherwise #f))

;;;;;;
;;; Traverse edited components

(def (function eo) map-edited-child-components (component visitor)
  (bind ((visitor (ensure-function visitor)))
    (map-child-components component (lambda (child)
                                      (when (edited-component? child)
                                        (funcall visitor child))))))

(def (function eo) map-edited-descendant-components (component visitor)
  (bind ((visitor (ensure-function visitor)))
    (map-edited-child-components component (lambda (child)
                                             (funcall visitor child)
                                             (map-edited-descendant-components child visitor)))))

(def (function eo) find-edited-child-component-if (predicate component &key (otherwise :error otherwise?))
  (bind ((predicate (ensure-function predicate)))
    (map-edited-child-components component (lambda (child)
                                             (when (funcall predicate child)
                                               (return-from find-edited-child-component-if child)))))
  (handle-otherwise (error "~S failed; predicate ~A, starting from ~A" 'find-edited-child-component-if predicate component)))

(def (function eo) find-edited-descendant-component-if (predicate component &key (otherwise :error otherwise?))
  (bind ((predicate (ensure-function predicate)))
    (map-edited-descendant-components component (lambda (descendant)
                                                  (when (funcall predicate descendant)
                                                    (return-from find-edited-descendant-component-if descendant)))))
  (handle-otherwise (error "~S failed; predicate ~A, starting from ~A" 'find-edited-descendant-component-if predicate component)))

(def (function e) has-edited-child-component? (component)
  (find-editable-child-component-if #'edited-component? component :otherwise #f))

(def (function e) has-edited-descendant-component? (component)
  (find-editable-descendant-component-if #'edited-component? component :otherwise #f))

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
  (app.dribble "Rendering component ~A" -self-)
  (with-component-environment -self-
    (ensure-refreshed -self-)
    (if (visible-component? -self-)
        (call-next-layered-method)
        (render-component-stub -self-))))

(def method lazily-rendered-component? ((self component))
  #f)

(def method to-be-rendered-component? ((self component))
  #f)

(def method mark-to-be-rendered-component ((self component))
  (map-child-components self #'mark-to-be-rendered-component))

(def method mark-to-be-rendered-component :before ((self component))
  (incremental.dribble "Calling MARK-TO-BE-RENDERED-COMPONENT on ~A" self))

(def method mark-rendered-component ((self component))
  (operation-not-supported "Cannot MARK-RENDERED-COMPONENT ~A, you may want to subclass RENDERABLE/MIXIN" self))

(def method mark-rendered-component :before ((self component))
  (incremental.dribble "Calling MARK-RENDERED-COMPONENT on ~A" self))

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

(def function ensure-refreshed (component)
  (when (to-be-refreshed-component? component)
    (refresh-component component))
  component)

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
    (component-slots-of (class-of self))))

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
  (operation-not-supported "Cannot clone ~A, you may want to subclass from CLONEABLE/COMPONENT" self))

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
