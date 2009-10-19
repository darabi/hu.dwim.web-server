;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/manager

(def (component e) t/manager (t/presentation tab-container-component)
  ())

(def (macro e) t/manager (type &body pages)
  `(make-instance 't/manager :component-value type :pages (list ,@pages)))

(def layered-method refresh-component :before ((self t/manager))
  (bind (((:slots component-value component-value pages) self))
    (setf pages (list* (tab-page/widget (icon switch-to-tab-page :label "Search")
                         (make-filter component-value))
                       (tab-page/widget (icon switch-to-tab-page :label "Create")
                         (make-maker component-value))
                       pages))))
