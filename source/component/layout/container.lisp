;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; container/layout

(def (component e) container/layout (layout/style contents/abstract frame-unique-id/mixin)
  ()
  (:documentation "A LAYOUT with several child COMPONENTs inside. The actual layout is set up on the remote side by style referring to its id."))

(def (macro e) container/layout ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'container/layout ,@args :contents (list ,@contents)))

(def render-xhtml container/layout
  (with-render-style/abstract (-self-)
    (render-contents-for -self-)))
