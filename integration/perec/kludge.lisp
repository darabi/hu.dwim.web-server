;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Kludges

;; TODO: KLUDGE: this redefines, but we are practical for now
(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or hu.dwim.perec:unbound null)))
             type))

;; TODO: KLUDGE: this redefines, but we are practical for now
(def function find-type-by-name (name)
  (or (find-class name #f)
      (hu.dwim.perec:find-type name)))

;; KLUDGE: TODO: redefined for now
(def function update-component-value-from-place (place component)
  (when (place-bound? place)
    (bind ((value (value-at-place place)))
      (setf (component-value-of component)
            (if (and (hu.dwim.perec::d-value-p value)
                     (hu.dwim.perec::single-d-value-p value))
                (hu.dwim.perec::single-d-value value)
                value)))))

;; KLUDGE: TODO: redefined for now
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
(def (function e) save-editing (editable &key (leave-editing #t))
  (assert (typep editable 'editable/mixin))
  ;; TODO: make this with-transaction part of a generic save-editing protocol dispatch
  (handler-bind ((hu.dwim.perec::persistent-constraint-violation (lambda (error)
                                                         (add-user-error editable "Adatösszefüggés hiba")
                                                         (abort-interaction)
                                                         (continue error))))
    (hu.dwim.rdbms:with-transaction
      (store-editing editable)
      (when (and leave-editing
                 (not (interaction-aborted-p)))
        (leave-editing editable))
      (if (interaction-aborted-p)
          (hu.dwim.rdbms:mark-transaction-for-rollback-only)
          (hu.dwim.rdbms:register-transaction-hook :after :commit
            (add-user-information editable "A változtatások elmentése sikerült"))))))

;; KLUDGE: TODO: redefined for now
(def method make-place-component-command-bar ((self standard-object-place-maker))
  (bind ((type (the-type-of self)))
    (make-instance 'command-bar/basic :commands (optional-list (when (hu.dwim.perec::null-subtype-p type)
                                                                 (make-set-place-to-nil-command self))
                                                               (when (or (initform-of self)
                                                                         (hu.dwim.perec::unbound-subtype-p type))
                                                                 (make-set-place-to-unbound-command self))
                                                               (make-set-place-to-find-instance-command self)
                                                               (make-set-place-to-new-instance-command self)))))
