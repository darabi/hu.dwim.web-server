;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (dmm:operation e) export-instance-operation (dmm::standard-operation)
  ())

;; TODO: shouldn't we put these customization on the dwim application layer?
(def layered-method make-export-command :around (format component (class standard-class) (prototype standard-object) (instance standard-object))
  (when (dmm::authorize-operation 'export-instance-operation :-entity- class :format format)
    (call-next-method)))
