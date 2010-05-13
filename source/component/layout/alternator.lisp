;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; alternator/layout

(def (component e) alternator/layout (standard/layout content/mixin)
  ((alternatives
    nil
    :type list))
  (:documentation "A /CLASS/HU.DWIM.WUI:COMPONENT with several child components, out of which only one is shown at a time."))

(def (macro e) alternator/layout ((&rest args &key &allow-other-keys) &body alternatives)
  (bind ((first-alternative (first alternatives)))
    (once-only (first-alternative)
      `(make-instance 'alternator/layout ,@args
                      :content ,first-alternative
                      :alternatives (list* ,first-alternative (list ,@(cdr alternatives)))))))

(def render-component alternator/layout
  (render-content-for -self-))

(def method switch-to-alternative ((component alternator/layout) alternative)
  (assert (member alternative (alternatives-of component) :test #'equal))
  (setf (content-of component) alternative))
