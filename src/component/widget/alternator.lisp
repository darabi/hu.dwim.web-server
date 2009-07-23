;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Alternator widget

(def (component e) alternator/widget (component-messages/widget alternator/layout hideable/mixin collapsible/mixin #+nil title/mixin context-menu/mixin command-bar/mixin frame-unique-id/mixin)
  ((initial-alternative-type t :type symbol)
   (default-alternative-type t :type symbol)))

(def (macro e) alternator/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'alternator/widget ,@args :alternatives (list ,@contents)))

(def refresh-component alternator/widget
  (bind (((:slots alternatives content) -self-))
    (unless content
      (setf content (find-initial-alternative-component -self-)))))

(def render-xhtml alternator/widget
  (with-render-style/abstract (-self- :element-name (if (typep (content-of -self-) 'reference/widget)
                                                        "span"
                                                        "div"))
    (render-context-menu-for -self-)
    (render-content-for -self-)))

(def method clone-component ((self alternator/widget))
  (prog1-bind clone (call-next-method)
    (setf (initial-alternative-type-of clone) (initial-alternative-type-of self)
          (default-alternative-type-of clone) (aif (content-of self)
                                                   (class-name (class-of it))
                                                   (default-alternative-type-of self)))))

(def layered-method make-context-menu-items ((component alternator/widget) class prototype value)
  (append (call-next-method)
          (list (make-menu-item (icon show-submenu :label "Nézet")
                                (make-switch-to-alternative-commands component class prototype value)))))

(def layered-method make-command-bar-commands ((component alternator/widget) class prototype value)
  (optional-list* (make-replace-with-alternative-command component (find-reference-alternative-component component :force #f)) (call-next-method)))

(def layered-method make-switch-to-alternative-commands ((component alternator/widget) class prototype value)
  (bind (((:read-only-slots alternatives) component))
    (delete nil
            (mapcar (lambda (alternative)
                      (make-replace-with-alternative-command component alternative))
                    alternatives))))

(def (generic e) make-replace-with-alternative-command (component alternative)
  (:method ((component alternator/widget) alternative)
    (bind ((prototype (class-prototype (if (computation? alternative)
                                           (the-class-of alternative)
                                           (class-of alternative))))
           (reference? (typep prototype 'reference/widget)))
      (make-replace-command (delay (content-of component)) alternative
                            :content (make-replace-with-alternative-command-content alternative prototype)
                            :visible (delay (and (not (has-edited-descendant-component-p (content-of component)))
                                                 (not (eq (if (computation? alternative)
                                                              (the-class-of alternative)
                                                              (class-of alternative))
                                                          (class-of (content-of component))))
                                                 (or (not reference?)
                                                     (find-ancestor-component-with-type (parent-component-of component) 'inspector/abstract))))
                            :ajax (ajax-of component)))))

(def (generic e) make-replace-with-alternative-command-content (alternative prototype)
  (:method (alternative (prototype component))
    (bind ((name (string-capitalize (substitute #\Space #\- (trim-suffix "-component" (string-downcase (class-name (class-of prototype))))))))
      (icon switch-to-alternative :label name :tooltip name)))

  (:method ((alternative string) (prototype string))
    (icon switch-to-alternative :label alternative :tooltip alternative))

  (:method (alternative (prototype reference/widget))
    (icon collapse-component)))

(def method join-editing ((alternator alternator/widget))
  (unless (typep (content-of alternator) 'reference/widget)
    (call-next-method)))

;;;;;;
;;; Alternative factory

;; TODO: factor it with one-time-computation
(def class* alternative-factory (computation)
  ((the-class :type standard-class :documentation "The class of the component that this factory produces.")
   (component nil :documentation "The component instance or NIL if not yet produced."))
  (:metaclass funcallable-standard-class))

(def (function e) find-alternative-component (component type &key (otherwise :error) (force #t))
  (bind ((alternatives (alternatives-of component)))
    (or (some (lambda (class)
                (awhen (find-if (lambda (alternative)
                                  (subtypep (if (computation? alternative)
                                                (the-class-of alternative)
                                                (class-of alternative))
                                            class))
                                alternatives)
                  (if force
                      (force it)
                      it)))
              (bind ((class (find-class type)))
                (class-precedence-list class)))
        (handle-otherwise otherwise))))

(def (function e) find-reference-alternative-component (component &key force)
  (find-alternative-component component 'reference/widget :force force))

(def (function e) find-initial-alternative-component (component)
  (find-alternative-component component (initial-alternative-type-of component)))

(def (function e) find-default-alternative-component (component)
  (find-alternative-component component (default-alternative-type-of component)))

(def (macro e) delay-alternative-component (type &body forms)
  `(aprog1 (make-instance 'alternative-factory :the-class (find-class ,type))
     (set-funcallable-instance-function it (delay (or (component-of it)
                                                      (setf (component-of it) (progn ,@forms)))))))

;; TODO: factor these into a single delay-alternative-component
(def (macro e) delay-alternative-component-with-initargs (type &rest args)
  `(delay-alternative-component ,type (make-instance ,type ,@args)))

(def (macro e) delay-alternative-reference (type target)
  `(delay-alternative-component ,type (make-alternative-reference ,type ,target)))

(def (function e) make-alternative-reference (type target)
  (prog1-bind reference (make-instance type :component-value target)
    (setf (ajax-of reference) (delay (ajax-of (parent-component-of reference)))
          (action-of reference) (make-action (execute-replace reference (delay (find-default-alternative-component (parent-component-of reference))))))))
