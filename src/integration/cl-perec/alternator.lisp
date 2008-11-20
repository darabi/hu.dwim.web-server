;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def dmm:operation switch-to-alternative-view-operation (dmm::standard-operation)
  ())

(def method make-replace-with-alternative-command :around ((component alternator-component) alternative)
  (when (or (typep (class-prototype (the-class-of alternative)) 'reference-component)
            (dmm::authorize-operation 'switch-to-alternative-view-operation))
    (call-next-method)))
