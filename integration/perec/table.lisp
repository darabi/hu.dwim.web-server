;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/inspector

(def layered-method make-alternatives ((component t/inspector) (class computed-class) (prototype hu.dwim.perec::table) (value hu.dwim.perec::table))
  (list* (make-instance 'table/sql/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (call-next-layered-method)))

;;;;;;
;;; table/sql/inspector

(def (component e) table/sql/inspector (inspector/style t/detail/inspector content/widget)
  ())

(def refresh-component table/sql/inspector
  (bind (((:slots content component-value) -self-))
    (setf content (hu.dwim.rdbms::format-sql-to-string (hu.dwim.rdbms::sql-create-table :name (hu.dwim.perec::name-of component-value)
                                                                                        :columns (hu.dwim.perec::columns-of component-value))))))
