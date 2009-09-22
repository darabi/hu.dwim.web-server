;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.

;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)













































;;;;;;
;;; Icon

(def (icon e) hide-component)

(def (icon e) show-component)

(def (icon e) refresh-component)

(def (icon e) select-component)

(def (icon e) switch-to-alternative)

(def layered-method make-refresh-component-command ((component refreshable/mixin) class prototype value)
  (command/widget ()
    (icon refresh-component)
    (make-component-action component
      (refresh-component component))))

(def (layered-function e) make-select-component-command (component class prototype value)
  (:method ((component selectable/mixin) class prototype value)
    (command/widget (:ajax (ajax-of (find-selection-component component)))
      (icon select-component)
      (make-component-action component
        (select-component component class prototype value)))))

(def layered-method make-context-menu-items ((component selectable/mixin) class prototype value)
  (optional-list* (make-menu-item (make-select-component-command component class prototype value) nil)
                  (call-next-method)))

(def function command-with-icon-name? (component name)
  (and (typep component 'command/widget)
       (bind ((content (content-of component)))
         (and (typep content 'icon/widget)
              (eq name (name-of content))))))

(def (generic e) find-command (component name)
  (:method ((self component) name)
    nil)

  (:method ((self context-menu/mixin) name)
    (or (call-next-method)
        (find-descendant-component (context-menu-of self)
                                   (lambda (descendant)
                                     (command-with-icon-name? descendant name)))))

  (:method ((self menu-bar/mixin) name)
    (or (call-next-method)
        (find-descendant-component (menu-bar-of self)
                                   (lambda (descendant)
                                     (command-with-icon-name? descendant name)))))

  (:method ((self command-bar/mixin) name)
    (or (call-next-method)
        (find-child-component (command-bar-of self)
                              (lambda (child)
                                (command-with-icon-name? child name))))))

(def (function e) render-hide-command-for (component)
  (render-component (command/widget ()
                      (icon hide-component :label nil)
                      (make-action
                        (hide-component component)))))






;;;;;;
;;; Icon

(def (icon e) begin-editing)

(def (icon e) save-editing)

(def (icon e) cancel-editing)

(def (icon e) store-editing)

(def (icon e) revert-editing)

(def layered-method make-context-menu-items ((component editable/mixin) (class standard-class) (prototype standard-object) (instance standard-object))
  (optional-list* (make-menu-item (icon menu :label "Edit")
                                  (make-editing-commands component class prototype instance))
                  (call-next-method)))

(def layered-method make-command-bar-commands ((component editable/mixin) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (when (editable-component? component)
            (list (make-save-editing-command component class prototype instance)
                  (make-cancel-editing-command component class prototype instance)))
          (call-next-method)))

;;;;;;
;;; Editable

(def layered-method make-begin-editing-command ((component editable/mixin) class prototype value)
  (command/widget (:visible (or (editable-component? component)
                                (delay (not (edited-component? component)))))
    (icon begin-editing)
    (make-component-action component
      (with-interaction component
        (begin-editing component)))))

(def layered-method make-save-editing-command (component class prototype value)
  (command/widget (:visible (delay (edited-component? component)))
    (icon save-editing)
    (make-component-action component
      (with-interaction component
        (save-editing component)))))

(def layered-method make-cancel-editing-command ((component editable/mixin) class prototype value)
  (command/widget (:visible (delay (edited-component? component)))
    (icon cancel-editing)
    (make-component-action component
      (with-interaction component
        (cancel-editing component)))))

(def layered-method make-store-editing-command ((component editable/mixin) class prototype value)
  (command/widget (:visible (delay (edited-component? component)))
    (icon store-editing)
    (make-component-action component
      (with-interaction component
        (save-editing component)))))

(def layered-method make-revert-editing-command ((component editable/mixin) class prototype instance)
  (command/widget (:visible (delay (edited-component? component)))
    (icon revert-editing)
    (make-component-action component
      (with-interaction component
        (revert-editing component)))))

(def layered-method make-editing-commands ((component editable/mixin) class prototype instance)
  (if (editable-component? component)
      (list (make-begin-editing-command component class prototype instance)
            (make-save-editing-command component class prototype instance)
            (make-cancel-editing-command component class prototype instance))
      (list (make-store-editing-command component class prototype instance)
            (make-revert-editing-command component class prototype instance))))

(def layered-method make-refresh-component-command ((component editable/mixin) class prototype instance)
  (command/widget (:visible (delay (not (edited-component? component)))
                   :ajax (ajax-of component))
    (icon refresh-component)
    (make-component-action component
      (refresh-component component))))

#|
(def function extract-primitive-component-place (component)
  (bind ((parent-component (parent-component-of component)))
    (typecase parent-component
      (place-inspector
       (bind ((place (place-of parent-component)))
         (when (typep place 'object-slot-place)
           (bind ((instance (instance-of place)))
             (values (class-of instance) instance (slot-of place))))))
      (t
       (setf parent-component (parent-component-of parent-component))
       (when (typep parent-component 'standard-slot-definition/mixin)
         (values (the-class-of parent-component) nil (slot-of parent-component)))))))


;;;;;;
;;; Command

(def layered-method make-context-menu-items ((component component) class prototype value)
  (append (call-next-method)
          (list (make-menu-item (icon menu :label #"context-menu.move-commands")
                                (make-move-commands component class prototype value)))))


;;;;;;
;;; Component value basic

(def (component e) component-value/basic (cloneable/mixin)
  ())

(def method clone-component :around ((self component-value/basic))
  ;; this must be done at the very last, after all primary method customization
  (prog1-bind clone (call-next-method)
    (setf (component-value-of clone) (component-value-of self))))



;;;;;;
;;; Command

(def layered-method make-hide-command ((component hideable/mixin) class prototype value)
  (command ()
    (icon hide-component)
    (make-component-action component
      (hide-component component))))

(def layered-method make-show-command ((component hideable/mixin) class prototype value)
  (command ()
    (icon show-component)
    (make-component-action component
      (show-component component))))

(def layered-method make-show-component-recursively-command ((component hideable/mixin) class prototype value)
  (command ()
    (icon show-component)
    (make-component-action component
      (show-component-recursively component))))

(def layered-method make-toggle-visiblity-command ((component hideable/mixin) class prototype value)
  (command ()
    (if (visible-component? component)
        (icon hide-component)
        (icon show-component))
    (make-component-action component
      (if (visible-component? component)
          (hide-component component)
          (show-component component)))))

(def layered-method make-context-menu-items ((component hideable/mixin) class prototype value)
  (list* (menu-item ()
             (icon menu :label "Show/Hide")
           (make-hide-command component class prototype value)
           (make-show-component-recursively-command component class prototype value))
         (call-next-method)))








;;;;;
;;; Closeable

(def (icon e) close-component)

(def layered-method make-close-component-command ((component closable/abstract) class prototype value)
  (command ()
    (icon close-component)
    (make-component-action component
      (close-component component class prototype value))))


;;;;;;
;;; Cloneable

(def (icon e) open-in-new-frame)

(def layered-method make-open-in-new-frame-command ((component component) class prototype value)
  (command (:delayed-content #t
            :js (lambda (href) `js(window.open ,href)))
    (icon open-in-new-frame)
    (make-component-action component
      (open-in-new-frame component class prototype value))))













;;;;;
;;; Exportable

(def layered-method make-context-menu-items ((component exportable/abstract) class prototype instance)
  (optional-list* (make-menu-item (icon menu :label "Export") (make-export-commands component class prototype instance))
                  (call-next-method)))

(def (icon e) export-text)

(def layered-method make-export-command ((format (eql :txt)) (component exportable/abstract) class prototype instance)
  (command/widget (:delayed-content #t :path (export-file-name format component))
    (icon export-text)
    (make-component-action component
      (export-text component))))

(def (icon e) export-csv)

(def layered-method make-export-command ((format (eql :csv)) (component exportable/abstract) class prototype instance)
  (command/widget (:delayed-content #t :path (export-file-name format component))
    (icon export-csv)
    (make-component-action component
      (export-csv component))))

(def (icon e) export-pdf)

(def special-variable *pdf-stream*)

(def layered-method make-export-command ((format (eql :pdf)) (component exportable/abstract) class prototype instance)
  (command/widget (:delayed-content #t :path (export-file-name format component))
    (icon export-pdf)
    (make-component-action component
      (export-pdf component))))

(def (icon e) export-odt)

(def layered-method make-export-command ((format (eql :odt)) (component exportable/abstract) class prototype instance)
  (command/widget (:delayed-content #t :path (export-file-name format component))
    (icon export-odt)
    (make-component-action component
      (export-odt component))))

(def (icon e) export-ods)

(def layered-method make-export-command ((format (eql :ods)) (component exportable/abstract) class prototype instance)
  (command/widget (:delayed-content #t :path (export-file-name format component))
    (icon export-ods)
    (make-component-action component
      (export-ods component))))



















(def (icon e) focus-in)

(def (icon e) focus-out)

(def layered-method make-focus-command ((component component) (class standard-class) (prototype standard-object) value)
  (bind ((original-component (delay (find-top-component-content component))))
    (make-replace-and-push-back-command original-component component
                                        (list :content (icon focus-in) :visible (delay (not (top-component-content? component))))
                                        (list :content (icon focus-out)))))























;;;;;;
;;; Default icons
;;; TODO: move the icons where they are actually used

(def (icon e) new)

(def (icon e) create)

(def (icon e) delete)

(def (icon e) close)

(def (icon e) back)

(def (icon e) expand)

(def (icon e) collapse)

(def (icon e) filter)

(def (icon e) find)

(def (icon e) set-to-nil)

(def (icon e) set-to-unbound)

(def (icon e) select)


(def (icon e) finish)

(def (icon e) cancel)













(def layered-method make-menu-bar ((component menu-bar/mixin) class prototype value)
  (make-instance 'menu-bar/basic :menu-items (make-menu-bar-items component class prototype value)))










;;;;;;
;;; context menu

(def layered-method make-context-menu ((component context-menu/mixin) class prototype value)
  (labels ((make-menu-items (elements)
             (iter (for element :in elements)
                   (collect (etypecase element
                              (cons (make-menu-item (icon menu) (make-menu-items element)))
                              (command/basic (make-menu-item element nil))
                              (menu-item/basic
                               (setf (menu-items-of element) (make-menu-items (menu-items-of element)))
                               element))))))
    (make-instance 'context-menu/basic
                   :target component
                   :content (icon show-context-menu)
                   :menu-items (make-menu-items (make-context-menu-items component class prototype value)))))















;;;;; TODO

;;;;;;
;;; Header mixin

#+nil
(def (component e) header/basic (widget/basic title/mixin context-menu/mixin collapsible/mixin)
  ()
  (:documentation "A COMPONENT with a HEADER."))
;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Inspector

(def method make-inspector (type &rest args &key &allow-other-keys)
  "A TYPE specifier is either
     - a primitive type name such as boolean, integer, string
     - a parameterized type specifier such as (integer 100 200) 
     - a compound type specifier such as (or null string)
     - a type alias refering to a parameterized or compound type such as standard-text
     - a CLOS class name such as standard-object or audited-object
     - a CLOS type instance parsed from a compound type specifier such as #<INTEGER-TYPE 0x1232112>"
  (bind (((component-type &rest additional-args)
          (ensure-list (find-inspector-type-for-type type))))
    (unless (subtypep component-type 'alternator/basic)
      (remove-from-plistf args :initial-alternative-type))
    (apply #'make-instance component-type
           (append args additional-args
                   (when (subtypep component-type 'primitive-component)
                     (list :the-type type))))))

(def (generic e) find-inspector-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'boolean/inspector)

  (:method ((type (eql 'components)))
    `(standard-object-list/inspector :the-class ,(find-class 'component)))

  (:method ((type symbol))
    (find-inspector-type-for-type (find-type-by-name type)))

  (:method ((class (eql (find-class t))))
    't/inspector)

  (:method ((class built-in-class))
    (find-inspector-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

  (:method ((type cons))
    (find-inspector-type-for-compound-type type))

  (:method ((class structure-class))
    (find-inspector-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-inspector-type-for-prototype (class-prototype class))))

(def (function) find-inspector-type-for-compound-type (type)
  (find-inspector-type-for-compound-type* (first type) type))

(def (generic e) find-inspector-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    `(member/inspector :possible-values ,(rest type)))

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-inspector-type-for-type (first main-type))
          (find-inspector-type-for-type t))))

  (:method ((first (eql 'list)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/inspector :the-class ,(find-type-by-name main-type))
          'list-component)))

  (:method ((first (eql 'components)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/inspector :the-class ,(find-type-by-name main-type))
          'list-component))))

(def (function e) make-inspector-for-prototype (prototype &rest args &key &allow-other-keys)
  (apply #'make-instance (find-inspector-type-for-prototype prototype) args))

(def (generic e) find-inspector-type-for-prototype (prototype)
  (:method ((prototype t))
    't-maker)

  (:method ((prototype string))
    'string/inspector)

  (:method ((prototype symbol))
    'symbol/inspector)

  (:method ((prototype integer))
    'integer/inspector)

  (:method ((prototype float))
    'float/inspector)

  (:method ((prototype number))
    'number/inspector)

  (:method ((prototype local-time:timestamp))
    'timestamp/inspector)

  (:method ((prototype list))
    'list-component)

  (:method ((prototype standard-slot-definition))
    'standard-slot-definition-component)

  (:method ((prototype structure-class))
    'standard-class-component)

  (:method ((prototype standard-class))
    'standard-class-component)

  (:method ((prototype structure-object))
    'standard-object/inspector)

  (:method ((prototype standard-object))
    'standard-object/inspector))

;;;;;;
;;; Viewer

(def method make-viewer (value &key &allow-other-keys)
  (:method (value &rest args &key type &allow-other-keys)
    (remove-from-plistf args :type)
    (prog1-bind component
        (apply #'make-inspector
               (or type
                   (if (and (typep value 'proper-list)
                            (every-type-p 'standard-object value))
                       '(list standard-object)
                       (class-of value)))
               args)
      (setf (component-value-of component) value))))

;;;;;;
;;; Editor

(def (generic e) make-editor (value &key &allow-other-keys)
  (:method (value &rest args &key &allow-other-keys)
    (prog1-bind component
        (apply #'make-viewer value args)
      (begin-editing component))))

;;;;;;
;;; Filter

(def method make-filter (type &rest args &key &allow-other-keys)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-filter-type-for-type type))))
    (unless (subtypep component-type 'alternator/basic)
      (remove-from-plistf args :initial-alternative-type))
    (prog1-bind component (apply #'make-instance component-type
                                 (append args additional-args
                                         (when (subtypep component-type 'primitive-component)
                                           (list :the-type type))))
      (when (typep component 'editable/mixin)
        (begin-editing component)))))

(def (generic e) find-filter-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'boolean-filter)

  (:method ((type (eql 'components)))
    't-filter)

  (:method ((type symbol))
    (find-filter-type-for-type (find-type-by-name type)))

  (:method ((class built-in-class))
    (find-filter-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

  (:method ((type cons))
    (find-filter-type-for-compound-type type))

  (:method ((class structure-class))
    (find-filter-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-filter-type-for-prototype (class-prototype class))))

(def function find-filter-type-for-compound-type (type)
  (find-filter-type-for-compound-type* (first type) type))

(def (generic e) find-filter-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    `(member-filter :possible-values ,(rest type)))

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-filter-type-for-type (first main-type))
          (find-filter-type-for-type t))))

  (:method ((first (eql 'components)) (type cons))
    't-filter))

(def (generic e) find-filter-type-for-prototype (prototype)
  (:method ((prototype t))
    't-filter)

  (:method ((prototype string))
    'string-filter)

  (:method ((prototype symbol))
    'symbol-filter)

  (:method ((prototype integer))
    'integer-filter)

  (:method ((prototype float))
    'float-filter)

  (:method ((prototype number))
    'number-filter)

  (:method ((prototype local-time:timestamp))
    'timestamp-filter)

  (:method ((prototype structure-object))
    `(standard-object-filter :the-class ,(class-of prototype)))

  (:method ((prototype standard-object))
    `(standard-object-filter :the-class ,(class-of prototype))))

;;;;;;
;;; Maker

(def method make-maker (type &rest args &key &allow-other-keys)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-maker-type-for-type type))))
    (unless (subtypep component-type 'alternator/basic)
      (remove-from-plistf args :initial-alternative-type))
    (prog1-bind component
        (apply #'make-instance component-type
               (append args additional-args
                       (when (subtypep component-type 'primitive-component)
                         (list :the-type type))))
      (when (typep component 'editable/mixin)
        (begin-editing component)))))

(def (generic e) find-maker-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'boolean-maker)

  (:method ((type (eql 'components)))
    't-maker)

  (:method ((type symbol))
    (find-maker-type-for-type (find-type-by-name type)))

  (:method ((class built-in-class))
    (find-maker-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

  (:method ((type cons))
    (find-maker-type-for-compound-type type))

  (:method ((class structure-class))
    (find-maker-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-maker-type-for-prototype (class-prototype class))))

(def function find-maker-type-for-compound-type (type)
  (find-maker-type-for-compound-type* (first type) type))

(def (generic e) find-maker-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-maker-type-for-type (first main-type))
          (find-maker-type-for-type t))))

  (:method ((first (eql 'components)) (type cons))
    't-maker))

(def (generic e) find-maker-type-for-prototype (prototype)
  (:method ((prototype t))
    't-maker)

  (:method ((prototype string))
    'string-maker)

  (:method ((prototype symbol))
    'symbol-maker)

  (:method ((prototype integer))
    'integer-maker)

  (:method ((prototype float))
    'float-maker)

  (:method ((prototype number))
    'number-maker)

  (:method ((prototype local-time:timestamp))
    'timestamp-maker)

  (:method ((instance structure-object))
    `(standard-object-maker :the-class ,(class-of instance)))

  (:method ((instance standard-object))
    `(standard-object-maker :the-class ,(class-of instance))))

;;;;;;
;;; Place inspector

(def (function e) make-place-inspector (type &rest args)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-place-inspector-type-for-type type))))
    (apply #'make-instance component-type (append args additional-args))))

(def (function e) make-special-variable-place-inspector (name &key (type t))
  (make-place-inspector type :place (make-special-variable-place name :type type)))

(def (macro e) make-lexical-variable-place-inspector (name &key (type t))
  `(make-place-inspector ,type :place (make-lexical-variable-place ,name :type ,type)))

(def (function e) make-standard-object-object-slot-place-inspector (instance slot)
  (bind ((place (make-object-slot-place instance slot)))
    (make-place-inspector (place-type place) :place place)))

(def (generic e) find-place-inspector-type-for-type (type)
  (:method (type)
    'place/inspector)

  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'place/inspector)

  (:method ((type (eql 'components)))
    'standard-object-place/inspector)
  
  (:method ((type symbol))
    (find-place-inspector-type-for-type (find-type-by-name type)))

  (:method ((type cons))
    (find-place-inspector-type-for-compound-type type))

  (:method ((class structure-class))
    (find-place-inspector-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-place-inspector-type-for-prototype (class-prototype class))))

(def function find-place-inspector-type-for-compound-type (type)
  (find-place-inspector-type-for-compound-type* (first type) type))

(def (generic e) find-place-inspector-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-place-inspector-type-for-type (first main-type))
          (find-place-inspector-type-for-type t))))

  (:method ((first (eql 'components)) (type cons))
    (find-place-inspector-type-for-type (second type))))

(def (generic e) find-place-inspector-type-for-prototype (prototype)
  (:method ((prototype structure-object))
    'standard-object-place/inspector)

  (:method ((prototype standard-object))
    'standard-object-place/inspector))

;;;;;;
;;; Place maker

(def (function e) make-place-maker (type &rest args)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-place-maker-type-for-type type))))
    (apply #'make-instance component-type :the-type type (append args additional-args))))

(def (generic e) find-place-maker-type-for-type (type)
  (:method (type)
    'place-maker)

  (:method ((type null))
    (error "NIL is not a valid type here"))
  
  (:method ((type (eql 'boolean)))
    'place-maker)
  
  (:method ((type symbol))
    (find-place-maker-type-for-type (find-type-by-name type)))

  (:method ((type cons))
    (find-place-maker-type-for-compound-type type))

  (:method ((class structure-class))
    (find-place-maker-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-place-maker-type-for-prototype (class-prototype class))))

(def function find-place-maker-type-for-compound-type (type)
  (find-place-maker-type-for-compound-type* (first type) type))

(def (generic e) find-place-maker-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-place-maker-type-for-type (first main-type))
          (find-place-maker-type-for-type t)))))

(def (generic e) find-place-maker-type-for-prototype (prototype)
  (:method ((prototype structure-object))
    'standard-object-place-maker)

  (:method ((prototype standard-object))
    'standard-object-place-maker))

;;;;;;
;;; Place filter

(def (function e) make-place-filter (type &rest args)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-place-filter-type-for-type type))))
    (apply #'make-instance component-type :the-type type (append args additional-args))))

(def (generic e) find-place-filter-type-for-type (type)
  (:method (type)
    'place-filter)

  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'place-filter)
  
  (:method ((type symbol))
    (find-place-filter-type-for-type (find-type-by-name type)))

  (:method ((type cons))
    (find-place-filter-type-for-compound-type type))

  (:method ((class structure-class))
    (find-place-filter-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-place-filter-type-for-prototype (class-prototype class))))

(def function find-place-filter-type-for-compound-type (type)
  (find-place-filter-type-for-compound-type* (first type) type))

(def (generic e) find-place-filter-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    'place-filter)

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-place-filter-type-for-type (first main-type))
          (find-place-filter-type-for-type t)))))

(def (generic e) find-place-filter-type-for-prototype (prototype)
  (:method ((prototype structure-object))
    'standard-object-place-filter)

  (:method ((prototype standard-object))
    'standard-object-place-filter))

;;;;;;
;;; Utility

;; TODO: KLUDGE: is this really this simple?
(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or null)))
             type))

(def function null-subtype-p (type)
  (subtypep 'null type))














;;;;;;
;;; List viewer

(def (component e) list/viewer (viewer/basic list/widget)
  ())

;;;;;;
;;; List editor

(def (component e) list/editor (editor/basic list/widget)
  ())

(def layered-method make-context-menu-items ((component list/editor) class prototype value)
  (optional-list* (make-menu-item (make-add-list-element-command component class prototype value) nil)
                  (call-next-method)))

(def (icon e) add-list-element)

(def (layered-function e) make-add-list-element-command (component class prototype value)
  (:method ((component list/editor) class prototype value)
    (command/widget (:ajax (ajax-of component))
      (icon add-list-element)
      (make-component-action component
        (appendf (contents-of component) (list (add-list-element component class prototype value)))))))

(def (generic e) add-list-element (component class prototype value))

;;;;;;
;;; List inspector

(def (component e) list/inspector (inspector/basic list/editor list/viewer)
  ())

(def (macro e) list/inspector ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'list/inspector ,@args :component-value (list ,@forms)))

(def refresh-component list/inspector
  (bind (((:slots contents component-value) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-)))
    (if contents
        (foreach [setf (component-value-of !1) !2] contents component-value)
        (setf contents (mapcar [make-list/element -self- dispatch-class dispatch-prototype !1] component-value)))))



































;; TODO: move this to element/editor
(def layered-method make-context-menu-items ((component element/editor) class prototype value)
  (optional-list* (make-menu-item (make-remove-list-element-command component class prototype value) nil)
                  (make-menu-item (make-select-component-command component class prototype value) nil)
                  (call-next-method)))

(def (icon e) remove-list-element)

(def (layered-function e) make-remove-list-element-command (component class prototype value)
  (:method ((component element/editor) class prototype value)
    (command/widget (:ajax (ajax-of component))
      (icon remove-list-element)
      (make-component-action component
        (appendf (contents-of component) (list (remove-list-element component class prototype value)))))))

(def (generic e) remove-list-element (component class prototype value))

;;;;;;
;;; Element viewer

(def (component e) element/viewer (viewer/basic element/widget)
  ())

;;;;;;
;;; Element editor

(def (component e) element/editor (editor/basic element/widget)
  ())

;;;;;;
;;; Element inspector

(def (component e) element/inspector (inspector/basic element/widget)
  ())


|#
