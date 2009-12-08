;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Kludges

(def (class* e) application-with-perec-support ()
  ())

(def method call-in-application-environment :around ((application application-with-perec-support) session thunk)
  (hu.dwim.meta-model::with-model-database
    (hu.dwim.perec::with-readonly-transaction
      (hu.dwim.perec:with-new-compiled-query-cache
        (call-next-method)))))

;; TODO: KLUDGE: move
(def method reuse-component-value ((component component) (class standard-class) (prototype object-slot-place) (value object-slot-place))
  (bind ((instance (instance-of value))
         (class (class-of instance))
         (reused-value (reuse-component-value component class (class-prototype class) instance)))
    (unless (eq value reused-value)
      (setf (instance-of value) reused-value))
    value))

;; TODO: KLUDGE: move
(def method reuse-component-value ((component component) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) (value hu.dwim.perec::persistent-object))
  (unless (eq value (class-prototype class))
    (hu.dwim.perec::load-instance value)))

;; KLUDGE: TODO: redefined for now
#+nil
(def function update-component-value-from-place (place component)
  (when (place-bound? place)
    (bind ((value (value-at-place place)))
      (setf (component-value-of component)
            (if (and (hu.dwim.perec::d-value-p value)
                     (hu.dwim.perec::single-d-value-p value))
                (hu.dwim.perec::single-d-value value)
                value)))))

;; KLUDGE: TODO: redefined for now
#+nil
(def layered-function render-icon-label (icon label)
  (:method (icon label)
    ;; TODO: use a flag in the component to mark the label as important (so cannot be hidden) and forget about typep here
    ;; TODO: factor this out into a preferences accessor function
    (when (and (hu.dwim.meta-model::has-authenticated-session)
               (parent-component-of icon)
               (typep (parent-component-of (parent-component-of icon)) 'command-bar/basic))
      (bind ((effective-subject (hu.dwim.meta-model::current-effective-subject))
             (subject-preferences (when effective-subject
                                    (hu.dwim.meta-model::subject-preferences-of effective-subject))))
        (when (and subject-preferences
                   ;; TODO use some preferences framework that can fall back to defaults unless overridden
                   ;; KLUDGE dmm dependency
                   (slot-boundp subject-preferences 'hu.dwim.meta-model::display-command-labels)
                   (not (hu.dwim.meta-model:display-command-labels? subject-preferences)))
          (return-from render-icon-label nil))))
    label))

;; KLUDGE: TODO: redefined for now
(def method save-editing :around ((self editable/mixin))
  ;; TODO: make this with-transaction part of a generic save-editing protocol dispatch
  (handler-bind ((hu.dwim.perec::persistent-constraint-violation (lambda (error)
                                                                   (add-component-error-message self "Adatösszefüggés hiba")
                                                                   (abort-interaction)
                                                                   (continue error))))
    (hu.dwim.rdbms:with-transaction
       (call-next-method)
       (if (interaction-aborted?)
           (hu.dwim.rdbms:mark-transaction-for-rollback-only)
           (hu.dwim.rdbms:register-transaction-hook :after :commit
             (add-component-information-message self "A változtatások elmentése sikerült"))))))

;; KLUDGE: TODO: redefined for now
#+nil
(def method make-place-component-command-bar ((self standard-object-place-maker))
  (bind ((type (the-type-of self)))
    (make-instance 'command-bar/basic :commands (optional-list (when (hu.dwim.perec::null-subtype-p type)
                                                                 (make-set-place-to-nil-command self))
                                                               (when (or (initform-of self)
                                                                         (hu.dwim.perec::unbound-subtype-p type))
                                                                 (make-set-place-to-unbound-command self))
                                                               (make-set-place-to-find-instance-command self)
                                                               (make-set-place-to-new-instance-command self)))))

(in-package :hu.dwim.perec)

(def persistent-type hu.dwim.wui::html-text (&optional maximum-length)
  "Formatted text that may contain various fonts, styles and colors as in XHTML."
  (declare (ignore maximum-length))
  `(and text
        (satisfies hu.dwim.wui::html-text?)))

(defmapping hu.dwim.wui::html-text (if (consp normalized-type)
                                       (sql-character-varying-type :size (maximum-length-of (parse-type normalized-type)))
                                       (sql-character-large-object-type))
  ;; TODO do some sanity check for maximum-length when provided
  'identity-reader
  'identity-writer)

(pushnew 'hu.dwim.wui::html-text hu.dwim.perec::*canonical-types*)
