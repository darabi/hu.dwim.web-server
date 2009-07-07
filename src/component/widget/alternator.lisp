;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Alternator mixin

(def (component e) alternator/mixin (content/mixin)
  ((initial-alternative-type 'detail-component :type symbol)
   (default-alternative-type 'detail-component :type symbol)
   (alternatives nil :type list)))

(def refresh-component alternator/mixin
  (bind (((:slots alternatives content) -self-)
         (value (component-value-of -self-))
         (class (component-dispatch-class -self-)))
    (unless alternatives
      (setf alternatives (make-alternatives -self- class (class-prototype class) value)))
    (unless content
      (setf content (find-initial-alternative-component -self-)))
    (dolist (alternative alternatives)
      (when-bind component (component-of alternative)
        (setf (component-value-of component) value)))))

(def method clone-component ((self alternator/mixin))
  (prog1-bind clone (call-next-method)
    (setf (initial-alternative-type-of clone) (initial-alternative-type-of self)
          (default-alternative-type-of clone) (aif (content-of self)
                                                   (class-name (class-of it))
                                                   (default-alternative-type-of self)))))

(def layered-method make-context-menu-items ((component alternator/mixin) class prototype value)
  (append (call-next-method)
          (list (make-menu-item (icon menu :label "Nézet")
                                (make-switch-to-alternative-commands component class prototype value)))))

(def layered-method make-command-bar-commands ((component alternator/mixin) class prototype value)
  (optional-list* (make-replace-with-alternative-command component (find-reference-alternative-component component :force #f)) (call-next-method)))

(def layered-method make-switch-to-alternative-commands ((component alternator/mixin) class prototype value)
  (bind (((:read-only-slots alternatives) component))
    (delete nil
            (mapcar (lambda (alternative)
                      (make-replace-with-alternative-command component alternative))
                    alternatives))))

(def (generic e) make-replace-with-alternative-command (component alternative)
  (:method ((component alternator/mixin) alternative)
    (bind ((prototype (class-prototype (the-class-of alternative)))
           (reference? (typep prototype 'reference-component)))
      (make-replace-command (delay (content-of component)) alternative
                            :content (make-replace-with-alternative-command-icon prototype)
                            :visible (delay (and (not (has-edited-descendant-component-p (content-of component)))
                                                 (not (eq (the-class-of alternative) (class-of (content-of component))))
                                                 (or (not reference?)
                                                     (find-ancestor-component-with-type (parent-component-of component) 'inspector/abstract))))
                            :ajax (ajax-of component)))))

(def (generic e) make-replace-with-alternative-command-icon (prototype)
  (:method ((prototype component))
    (bind ((name (string-capitalize (substitute #\Space #\- (trim-suffix "-component" (string-downcase (class-name (class-of prototype))))))))
      (icon view :label name :tooltip name)))

  (:method ((prototype reference-component))
    (icon collapse)))

(def method join-editing ((alternator alternator/mixin))
  (unless (typep (content-of alternator) 'reference-component)
    (call-next-method)))

;;;;;;
;;; Alternator component

(def (component e) alternator/widget (visibility/mixin collapsible/mixin title/mixin context-menu/mixin commands/mixin alternator/mixin id/mixin component-messages/widget)
  ())

(def render-xhtml alternator/widget
  (render-content-for -self-))

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
