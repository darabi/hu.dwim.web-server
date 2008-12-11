;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def dmm:operation replace-with-alternative-operation (dmm::standard-operation)
  ())

(def method make-replace-with-alternative-command :around ((component alternator-component) alternative)
  (when (or (subtypep (the-class-of alternative) 'reference-component)
            (dmm::authorize-operation 'replace-with-alternative-operation))
    (call-next-method)))
