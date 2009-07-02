;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; TODO: split to the other files and factor with that code

;;;;;;
;;; Reference viewer

(def (component e) reference/viewer (viewer/basic command/widget)
  ())

(def refresh-component reference/viewer
  (bind (((:slots content component-value) -self-))
    (setf content (make-reference/content -self- (component-dispatch-class -self-) (component-dispatch-prototype -self-) component-value))))

(def (generic e) make-reference/content (component class prototype value)
  (:method ((component reference/viewer) class prototype value)
    (princ-to-string value))

  (:method ((component reference/viewer) (class standard-class) (prototype standard-object) (value standard-object))
    (localized-instance-reference-string value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard object tree viewer

(def (component e) standard-object/tree/viewer (viewer/basic tree/widget)
  ())

(def method make-tree/root-node ((component standard-object/tree/viewer) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/node/viewer :component-value value))

;;;;;;
;;; Standard object node viewer

(def (component e) standard-object/node/viewer (viewer/basic node/widget)
  ())

(def method make-node/child-node ((component standard-object/node/viewer) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/node/viewer :component-value value))

(def method make-node/content ((component standard-object/node/viewer) (class standard-class) (prototype standard-object) (value standard-object))
  (localized-instance-reference-string value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard object tree level viewer

(def (component e) standard-object/tree-level/viewer (viewer/basic tree-level/widget)
  ())

(def method make-tree-level/path ((component standard-object/tree-level/viewer) (class standard-class) (prototype standard-class) (value list))
  (make-instance 'standard-object/tree-level/path/viewer :component-value value))

(def method collect-tree/children ((component standard-object/tree-level/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (sort (copy-list (class-direct-subclasses value)) #'string<
        :key (compose #'qualified-symbol-name #'class-name)))

;;;;;;
;;; Standard object tree level reference viewer

(def (component e) standard-object/tree-level/reference/viewer (reference/viewer)
  ())

(def refresh-component standard-object/tree-level/reference/viewer
  (bind (((:slots action component-value) -self-))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-with-type -self- 'standard-object/tree-level/viewer)) component-value)))))

;;;;;;
;;; Standard object tree level path viewer

(def (component e) standard-object/tree-level/path/viewer (viewer/basic path/widget)
  ())

(def method component-dispatch-class ((self standard-object/tree-level/path/viewer))
  (class-of (first (component-value-of self))))

(def method make-path/content ((component standard-object/tree-level/path/viewer) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/reference/viewer :component-value value))

;;;;;;
;;; Standard object tree level tree viewer

(def (component e) standard-object/tree-level/tree/viewer (standard-object/tree/viewer)
  ())

(def method make-tree/root-node ((component standard-object/tree-level/tree/viewer) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/node/viewer :component-value value))

;;;;;;
;;; Standard object tree level node viewer

(def (component e) standard-object/tree-level/node/viewer (standard-object/node/viewer)
  ())

(def method make-node/child-node ((component standard-object/tree-level/node/viewer) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/node/viewer :component-value value))

(def method make-node/content ((component standard-object/tree-level/node/viewer) (class standard-class) (prototype standard-object) (value standard-object))
  (make-instance 'standard-object/tree-level/reference/viewer :component-value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard class tree viewer

(def (component e) standard-class/tree/viewer (standard-object/tree/viewer)
  ())

(def (macro e) standard-class/tree/viewer ((&rest args &key &allow-other-keys) &body class)
  `(make-instance 'standard-class/tree/viewer ,@args :component-value ,(the-only-element class)))

(def method make-tree/root-node ((component standard-class/tree/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/node/viewer :component-value value))

;;;;;;
;;; Standard class node viewer

(def (component e) standard-class/node/viewer (standard-object/node/viewer)
  ())

(def method make-node/child-node ((component standard-class/node/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/node/viewer :component-value value))

(def method collect-tree/children ((component standard-class/node/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (sort (class-direct-subclasses value) #'string<
        :key (compose #'qualified-symbol-name #'class-name)))

(def method make-node/content ((component standard-class/node/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (qualified-symbol-name (class-name value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Standard class tree level viewer

(def (component e) standard-class/tree-level/viewer (standard-object/tree-level/viewer)
  ())

(def (macro e) standard-class/tree-level/viewer ((&rest args &key &allow-other-keys) &body class)
  `(make-instance 'standard-class/tree-level/viewer ,@args :component-value ,(the-only-element class)))

(def method find-tree/parent ((component standard-class/tree-level/viewer) (class standard-class) (prototype standard-class) value)
  (second (class-precedence-list value)))

(def method make-tree-level/path ((component standard-class/tree-level/viewer) (class standard-class) (prototype standard-class) (value list))
  (make-instance 'standard-class/tree-level/path/viewer :component-value value))

(def method make-tree-level/previous-sibling ((component standard-class/tree-level/viewer) (class standard-class) (prototype standard-class) value)
  (make-instance 'standard-class/tree-level/reference/viewer :component-value value))

(def method make-tree-level/next-sibling ((component standard-class/tree-level/viewer) (class standard-class) (prototype standard-class) value)
  (make-instance 'standard-class/tree-level/reference/viewer :component-value value))

(def method make-tree-level/descendants ((component standard-class/tree-level/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/tree/viewer :component-value value))

(def method make-tree-level/node ((component standard-class/tree-level/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/reference/viewer :component-value value))

;;;;;;
;;; Standard class tree level class reference

(def (component e) standard-class/tree-level/reference/viewer (viewer/basic command/widget)
  ())

(def refresh-component standard-class/tree-level/reference/viewer
  (bind (((:slots content action component-value) -self-))
    (setf content (qualified-symbol-name (class-name component-value)))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-with-type -self- 'standard-class/tree-level/viewer)) component-value)))))

;;;;;;
;;; Standard class tree level path viewer

(def (component e) standard-class/tree-level/path/viewer (standard-object/tree-level/path/viewer)
  ())

(def method component-dispatch-class ((self standard-class/tree-level/path/viewer))
  (class-of (first (component-value-of self))))

(def method make-path/content ((component standard-class/tree-level/path/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/reference/viewer :component-value value))

;;;;;;
;;; Standard class tree level tree viewer

(def (component e) standard-class/tree-level/tree/viewer (standard-class/tree/viewer)
  ())

(def method make-tree/root-node ((component standard-class/tree-level/tree/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/node/viewer :component-value value))

;;;;;;
;;; Standard class tree level node viewer

(def (component e) standard-class/tree-level/node/viewer (standard-class/node/viewer)
  ())

(def method make-node/child-node ((component standard-class/tree-level/node/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/node/viewer :component-value value))

(def method make-node/content ((component standard-class/tree-level/node/viewer) (class standard-class) (prototype standard-class) (value standard-class))
  (make-instance 'standard-class/tree-level/reference/viewer :component-value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;
;;; Book tree level viewer

(def (component e) book/tree-level/viewer (standard-object/tree-level/viewer)
  ())

(def (macro e) book/tree-level/viewer ((&rest args &key &allow-other-keys) &body book)
  `(make-instance 'book/tree-level/viewer ,@args :component-value ,(the-only-element book)))

(def method make-tree-level/path ((component book/tree-level/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value list))
  (make-instance 'standard-object/tree-level/path/viewer :component-value value))

(def method make-path/content ((component standard-object/tree-level/path/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 'standard-object/tree-level/reference/viewer :component-value value))

(def method make-tree-level/previous-sibling ((component book/tree-level/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) value)
  (make-instance 'standard-object/tree-level/reference/viewer :component-value value))

(def method make-tree-level/next-sibling ((component book/tree-level/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) value)
  (make-instance 'standard-object/tree-level/reference/viewer :component-value value))

(def method make-tree-level/descendants ((component book/tree-level/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 'standard-object/tree-level/tree/viewer :component-value value))

(def method make-tree-level/node ((component book/tree-level/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 'standard-object/tree-level/reference/viewer :component-value value))

(def method find-tree/parent ((component standard-object/tree-level/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (parent-book-element-of value))

(def method collect-tree/children ((component book/tree-level/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (filter-if (of-type 'title-mixin) (contents-of value)))

(def method collect-tree/children ((component standard-object/node/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (filter-if (of-type 'title-mixin) (contents-of value)))

(def method make-reference/content ((component standard-object/tree-level/reference/viewer) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (title-of value))
