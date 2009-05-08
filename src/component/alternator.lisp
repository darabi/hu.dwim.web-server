;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Alternator component mixin

(def component alternator-component-mixin (content-component)
  ((initial-alternative-type 'detail-component :type symbol)
   (default-alternative-type 'detail-component :type symbol)
   (alternatives-factory #'make-alternatives :type function)
   (alternatives nil :type list)))

(def refresh alternator-component-mixin
  (bind (((:slots alternatives content) -self-)
         (value (component-value-of -self-))
         (class (component-dispatch-class -self-)))
    (setf  alternatives (funcall (alternatives-factory-of -self-) -self- class (class-prototype class) value)
           content (if content
                       (or (find-alternative-component -self- (type-of content))
                           (find-default-alternative-component -self-))
                       (find-initial-alternative-component -self-)))
    (dolist (alternative alternatives)
      (when-bind component (component-of alternative)
        (setf (component-value-of component) value)))))

(def layered-method clone-component ((self alternator-component-mixin))
  (prog1-bind clone (call-next-method)
    (setf (default-alternative-type-of clone) (aif (content-of self)
                                                   (class-name (class-of it))
                                                   (default-alternative-type-of self)))))

(def layered-method make-context-menu-commands ((component alternator-component-mixin) class prototype value)
  (append (call-next-method) (make-alternative-commands component class prototype value)))

(def layered-method make-command-bar-commands ((component alternator-component-mixin) class prototype value)
  (optional-list* (make-replace-with-alternative-command component (find-reference-alternative-component component :force #f)) (call-next-method)))

(def (layered-function e) make-alternative-commands (component class prototype value)
  (:method ((component alternator-component-mixin) class prototype value)
    (bind (((:read-only-slots alternatives) component))
      (delete nil
              (mapcar (lambda (alternative)
                        (make-replace-with-alternative-command component alternative))
                      alternatives)))))

(def (layered-function e) make-alternatives (component class prototyp value))

(def (generic e) make-replace-with-alternative-command (component alternative)
  (:method ((component alternator-component-mixin) alternative)
    (bind ((prototype (class-prototype (the-class-of alternative)))
           (reference? (typep prototype 'reference-component)))
      (make-replace-command (delay (content-of component)) alternative
                            :content (make-replace-with-alternative-command-icon prototype)
                            :visible (delay (and (not (has-edited-descendant-component-p (content-of component)))
                                                 (not (eq (the-class-of alternative) (class-of (content-of component))))
                                                 (or (not reference?)
                                                     (find-ancestor-component-with-type (parent-component-of component) 'inspector-component))))
                            :ajax (ajax-id component)))))

(def (generic e) make-replace-with-alternative-command-icon (prototype)
  (:method ((prototype component))
    (bind ((name (string-capitalize (substitute #\Space #\- (trim-suffix "-component" (string-downcase (class-name (class-of prototype))))))))
      (icon view :label name :tooltip name)))

  (:method ((prototype reference-component))
    (icon collapse)))

(def method join-editing ((alternator alternator-component-mixin))
  (unless (typep (content-of alternator) 'reference-component)
    (call-next-method)))

;;;;;;
;;; Alternator component

(def component alternator-component (commands-component-mixin
                                     alternator-component-mixin
                                     title-component-mixin
                                     remote-identity-component-mixin
                                     user-message-collector-component-mixin)
  ())

(def render-xhtml alternator-component
  (bind (((:read-only-slots command-bar content id) -self-)
         (css-class (component-css-class -self-)))
    (if (typep content '(or reference-component primitive-component))
        <span (:id ,id :class ,css-class)
              ,(render-user-messages -self-)
              ,(call-next-method)>
        (progn
          <div (:id ,id :class ,css-class)
               ,(render-title -self-)
               ,(render-user-messages -self-)
               ,(call-next-method)
               ,(render command-bar)>
          `js(wui.setup-widget ,css-class ,id)))))

;;;;;;
;;; Alternative factory

(def class* alternative-factory (computation)
  ((the-class :type standard-class :documentation "The class of the component that this factory produces.")
   (component nil :documentation "The component instance or NIL if not yet produced."))
  (:metaclass funcallable-standard-class))

(def (function e) find-alternative-component (component type &key (otherwise :error) (force #t))
  (bind ((alternatives (alternatives-of component)))
    (or (some (lambda (class)
                (awhen (find-if (lambda (alternative)
                                  (subtypep (the-class-of alternative) class))
                                alternatives)
                  (if force
                      (force it)
                      it)))
              (bind ((class (find-class type)))
                (class-precedence-list class)))
        (handle-otherwise otherwise))))

(def (function e) find-reference-alternative-component (component &key force)
  (find-alternative-component component 'reference-component :force force))

(def (function e) find-initial-alternative-component (component)
  (find-alternative-component component (initial-alternative-type-of component)))

(def (function e) find-default-alternative-component (component)
  (find-alternative-component component (default-alternative-type-of component)))

(def (macro e) delay-alternative-component (type &body forms)
  `(aprog1 (make-instance 'alternative-factory :the-class (find-class ,type))
     (set-funcallable-instance-function it (delay (or (component-of it)
                                                      (setf (component-of it) (progn ,@forms)))))))

(def (macro e) delay-alternative-component-with-initargs (type &rest args)
  `(delay-alternative-component ,type (make-instance ,type ,@args)))

(def (macro e) delay-alternative-reference-component (type target)
  `(delay-alternative-component ,type (make-alternative-reference-component ,type ,target)))

(def (function e) make-alternative-reference-component (type target)
  (prog1-bind reference (make-instance type :target target)
    (setf (expand-command-of reference)
          (make-expand-reference-command reference (class-of target) target
                                         (delay (find-default-alternative-component (parent-component-of reference)))))))
