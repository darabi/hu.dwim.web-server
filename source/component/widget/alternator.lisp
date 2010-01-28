;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; alternator/widget

(def (component e) alternator/widget (component-messages/widget
                                      alternator/layout
                                      hideable/mixin
                                      collapsible/mixin
                                      context-menu/mixin
                                      command-bar/mixin
                                      frame-unique-id/mixin)
  ((initial-alternative-type t :type symbol)
   (default-alternative-type t :type symbol)))

(def (macro e) alternator/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'alternator/widget ,@args :alternatives (list ,@contents)))

(def refresh-component alternator/widget
  (bind (((:slots alternatives content) -self-))
    (unless content
      (setf content (find-initial-alternative-component -self-)))))

(def render-xhtml alternator/widget
  (bind (((:read-only-slots content) -self-))
    (with-render-style/abstract (-self- :element-name (if (typep content 'reference/widget)
                                                          "span"
                                                          "div"))
      (render-context-menu-for -self-)
      (render-component-messages-for -self-)
      (render-content-for -self-)
      (when (render-command-bar-for-alternative? content)
        (render-command-bar-for -self-)))))

(def method visible-child-component-slots ((component alternator/widget))
  (remove-slots (unless (render-command-bar-for-alternative? (content-of component))
                  '(command-bar))
                (call-next-method)))

(def generic render-command-bar-for-alternative? (component)
  (:method (component)
    #t)

  (:method ((component reference/widget))
    #f))

(def method clone-component ((self alternator/widget))
  (prog1-bind clone (call-next-method)
    (setf (initial-alternative-type-of clone) (initial-alternative-type-of self)
          (default-alternative-type-of clone) (aif (content-of self)
                                                   (class-name (class-of it))
                                                   (default-alternative-type-of self)))))

(def layered-method make-context-menu-items ((component alternator/widget) class prototype value)
  (append (call-next-method)
          (list (make-submenu-item (icon show-submenu :label "View")
                                   (make-switch-to-alternative-commands component class prototype value)))))

(def layered-method make-command-bar-commands ((component alternator/widget) class prototype value)
  (optional-list* (make-replace-with-alternative-command component (find-reference-alternative-component component)) (call-next-method)))

(def layered-method make-switch-to-alternative-commands ((component alternator/widget) class prototype value)
  (bind (((:read-only-slots alternatives) component))
    (delete nil
            (mapcar (lambda (alternative)
                      (make-replace-with-alternative-command component alternative))
                    alternatives))))

(def (generic e) make-replace-with-alternative-command (component alternative)
  (:method ((component alternator/widget) alternative)
    (bind ((prototype (class-prototype (if (computation? alternative)
                                           (component-class-of alternative)
                                           (class-of alternative))))
           (reference? (typep prototype 'reference/widget)))
      (make-instance 'command/widget
                     :action (make-action
                               (setf (default-alternative-type-of component) (type-of (content-of component)))
                               (execute-replace (content-of component) alternative))
                     :content (make-replace-with-alternative-command-content alternative prototype)
                     :visible (delay (and (not (has-edited-descendant-component-p (content-of component)))
                                          (not (eq (if (computation? alternative)
                                                       (component-class-of alternative)
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
    (icon collapse-to-reference)))

(def method join-editing ((alternator alternator/widget))
  (unless (typep (content-of alternator) 'reference/widget)
    (call-next-method)))

(def (icon e) expand-from-reference)

(def (icon e) collapse-to-reference)

(def (function e) find-alternative-component (component type &key (otherwise :error))
  (bind ((alternatives (alternatives-of component)))
    (or (some (lambda (class)
                (find-if [subtypep (class-of !1) class) alternatives))
              (class-precedence-list (find-class type)))
        (handle-otherwise otherwise))))

(def (function e) find-reference-alternative-component (component)
  (find-alternative-component component 'reference/widget))

(def (function e) find-initial-alternative-component (component)
  (find-alternative-component component (initial-alternative-type-of component)))

(def (function e) find-default-alternative-component (component)
  (find-alternative-component component (default-alternative-type-of component)))
