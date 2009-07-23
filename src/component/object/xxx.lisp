;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; TODO: split to the other files and factor with that code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard object tree inspector

(def (component e) standard-object/tree/inspector (inspector/basic tree/widget)
  ())

(def layered-method make-tree/root-node ((component standard-object/tree/inspector) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/node/inspector :component-value value))

;;;;;;
;;; Standard object node inspector

(def (component e) standard-object/node/inspector (inspector/basic node/widget)
  ())

(def layered-method make-node/child-node ((component standard-object/node/inspector) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/node/inspector :component-value value))

(def layered-method make-node/content ((component standard-object/node/inspector) (class standard-class) (prototype standard-object) (value standard-object))
  (localized-instance-name value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard object tree level inspector

(def (component e) standard-object/tree-level/inspector (inspector/basic tree-level/widget)
  ())

(def layered-method make-tree-level/path ((component standard-object/tree-level/inspector) (class standard-class) (prototype standard-class) (value list))
  (make-instance 'standard-object/tree-level/path/inspector :component-value value))

(def layered-method collect-tree/children ((component standard-object/tree-level/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (sort (copy-list (class-direct-subclasses value)) #'string<
        :key (compose #'qualified-symbol-name #'class-name)))

;;;;;;
;;; Standard object tree level reference inspector

(def (component e) standard-object/tree-level/reference/inspector (t/reference/inspector)
  ())

(def refresh-component standard-object/tree-level/reference/inspector
  (bind (((:slots action component-value) -self-))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-with-type -self- 'standard-object/tree-level/inspector)) component-value)))))

;;;;;;
;;; Standard object tree level path inspector

(def (component e) standard-object/tree-level/path/inspector (inspector/basic path/widget)
  ())

(def method component-dispatch-class ((self standard-object/tree-level/path/inspector))
  (class-of (first (component-value-of self))))

(def layered-method make-path/content ((component standard-object/tree-level/path/inspector) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/reference/inspector :component-value value))

;;;;;;
;;; Standard object tree level tree inspector

(def (component e) standard-object/tree-level/tree/inspector (standard-object/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component standard-object/tree-level/tree/inspector) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/node/inspector :component-value value))

;;;;;;
;;; Standard object tree level node inspector

(def (component e) standard-object/tree-level/node/inspector (standard-object/node/inspector)
  ())

(def layered-method make-node/child-node ((component standard-object/tree-level/node/inspector) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/node/inspector :component-value value))

(def layered-method make-node/content ((component standard-object/tree-level/node/inspector) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/reference/inspector :component-value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard class tree inspector

(def (component e) standard-class/tree/inspector (standard-object/tree/inspector)
  ())

(def (macro e) standard-class/tree/inspector ((&rest args &key &allow-other-keys) &body class)
  `(make-instance 'standard-class/tree/inspector ,@args :component-value ,(the-only-element class)))

(def layered-method make-tree/root-node ((component standard-class/tree/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/node/inspector :component-value value))

;;;;;;
;;; Standard class node inspector

(def (component e) standard-class/node/inspector (standard-object/node/inspector)
  ())

(def layered-method make-node/child-node ((component standard-class/node/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/node/inspector :component-value value))

(def layered-method collect-tree/children ((component standard-class/node/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (sort (class-direct-subclasses value) #'string<
        :key (compose #'qualified-symbol-name #'class-name)))

(def layered-method make-node/content ((component standard-class/node/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (qualified-symbol-name (class-name value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard class tree level inspector

(def (component e) standard-class/tree-level/inspector (standard-object/tree-level/inspector)
  ())

(def (macro e) standard-class/tree-level/inspector ((&rest args &key &allow-other-keys) &body class)
  `(make-instance 'standard-class/tree-level/inspector ,@args :component-value ,(the-only-element class)))

(def layered-method make-tree-level/path ((component standard-class/tree-level/inspector) (class standard-class) (prototype standard-class) (value list))
  (make-instance 'standard-class/tree-level/path/inspector :component-value value))

(def layered-method make-tree-level/previous-sibling ((component standard-class/tree-level/inspector) (class standard-class) (prototype standard-class) value)
  (make-instance 'standard-class/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/next-sibling ((component standard-class/tree-level/inspector) (class standard-class) (prototype standard-class) value)
  (make-instance 'standard-class/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/descendants ((component standard-class/tree-level/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/tree/inspector :component-value value))

(def layered-method make-tree-level/node ((component standard-class/tree-level/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/reference/inspector :component-value value))

;;;;;;
;;; Standard class tree level class reference

(def (component e) standard-class/tree-level/reference/inspector (inspector/basic command/widget)
  ())

(def refresh-component standard-class/tree-level/reference/inspector
  (bind (((:slots content action component-value) -self-))
    (setf content (qualified-symbol-name (class-name component-value)))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-with-type -self- 'standard-class/tree-level/inspector)) component-value)))))

;;;;;;
;;; Standard class tree level path inspector

(def (component e) standard-class/tree-level/path/inspector (standard-object/tree-level/path/inspector)
  ())

(def method component-dispatch-class ((self standard-class/tree-level/path/inspector))
  (class-of (first (component-value-of self))))

(def layered-method make-path/content ((component standard-class/tree-level/path/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/reference/inspector :component-value value))

;;;;;;
;;; Standard class tree level tree inspector

(def (component e) standard-class/tree-level/tree/inspector (standard-class/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component standard-class/tree-level/tree/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/node/inspector :component-value value))

;;;;;;
;;; Standard class tree level node inspector

(def (component e) standard-class/tree-level/node/inspector (standard-class/node/inspector)
  ())

(def layered-method make-node/child-node ((component standard-class/tree-level/node/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/node/inspector :component-value value))

(def layered-method make-node/content ((component standard-class/tree-level/node/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/reference/inspector :component-value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Book tree level inspector

(def (component e) book/tree-level/inspector (standard-object/tree-level/inspector)
  ())

(def (macro e) book/tree-level/inspector ((&rest args &key &allow-other-keys) &body book)
  `(make-instance 'book/tree-level/inspector ,@args :component-value ,(the-only-element book)))

(def layered-method make-tree-level/path ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value list))
  (make-instance 'standard-object/tree-level/path/inspector :component-value value))

(def layered-method make-path/content ((component standard-object/tree-level/path/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 'standard-object/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/previous-sibling ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) value)
  (make-instance 'standard-object/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/next-sibling ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) value)
  (make-instance 'standard-object/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/descendants ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 'standard-object/tree-level/tree/inspector :component-value value))

(def layered-method make-tree-level/node ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 'standard-object/tree-level/reference/inspector :component-value value))

(def layered-method collect-tree/children ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (filter-if (of-type 'title-mixin) (contents-of value)))

(def layered-method collect-tree/children ((component standard-object/node/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (filter-if (of-type 'title-mixin) (contents-of value)))

(def layered-method make-reference/content ((component standard-object/tree-level/reference/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (title-of value))
