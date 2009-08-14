;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customizations

(def layered-method collect-standard-object-detail-maker-slots ((component standard-object-detail-maker) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object))
  (bind ((excluded-slot-name
          (awhen (find-ancestor-component-with-type component 'standard-object-slot-value/inspector)
            (bind ((slot (slot-of it)))
              (when (typep slot 'hu.dwim.perec::persistent-association-end-effective-slot-definition)
                (slot-definition-name (hu.dwim.perec::other-association-end-of slot)))))))
    (remove-if (lambda (slot)
                 (or (hu.dwim.perec:persistent-object-internal-slot-p slot)
                     (eq (slot-definition-name slot) excluded-slot-name)))
               (call-next-method))))

(def layered-method collect-standard-object-detail-maker-slots ((component standard-object-detail-maker) (class hu.dwim.meta-model::entity) (prototype hu.dwim.perec::persistent-object))
  (filter-if (lambda (slot)
               (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::create-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

;; TODO: FIXME: this is the same signature?
(def layered-method create-instance ((component standard-object-maker) (class hu.dwim.perec::persistent-class))
  (handler-bind ((hu.dwim.perec::persistent-constraint-violation (lambda (error)
                                                         (add-user-error component "Adatösszefüggés hiba")
                                                         (abort-interaction)
                                                         (continue error))))
    (rdbms::with-transaction
      (prog1 (call-next-method)
        (when (interaction-aborted-p)
          (hu.dwim.rdbms:mark-transaction-for-rollback-only))))))

;; TODO: FIXME: this is the same signature?
(def layered-method create-instance ((component standard-object-maker) (class hu.dwim.perec::persistent-class))
  (prog1 (call-next-method)
    (unless (interaction-aborted-p)
      (rdbms:register-transaction-hook :after :commit
        (add-user-information component "Az új objektum létrehozása sikerült")))))

(def (method e) make-instance-using-initargs ((component standard-object-maker) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object))
  (handler-bind ((type-error (lambda (error)
                               (add-user-error (find-descendant-component component
                                                                          (lambda (descendant)
                                                                            (and (typep descendant 'place-maker)
                                                                                 (eq (hu.dwim.perec::slot-of error) (slot-of (parent-component-of descendant))))))
                                               "Nem megfelelő adat")
                               (abort-interaction)
                               (continue error))))
    (call-next-method)))
