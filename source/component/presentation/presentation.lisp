;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/presentation

(def (component e) component/presentation ()
  ()
  (:documentation "A presentation is the abstract base class of all TYPE related meta COMPONENTs. It presents a VALUE of TYPE in a way that is specific to the component. This class does not have any slots on purpose.
  - static input
    - value-type: type
  - volatile input
    - value: type
  - dispatch
    - dispatch-class: (class-of value)
    - dispatch-prototype: (class-prototype dispatch-class)
  - output
    - value: type"))

;;;;;;
;;; t/presentation

;; TODO: the order of superclasses here might be not what we want
;; it's weird that component-value/mixin comes before standard/component
;; but in the class precedence list of t/detail/presentation component-value/mixin must come before content/component
(def (component e) t/presentation (component/presentation component-value/mixin component-value-type/mixin standard/component)
  ())
