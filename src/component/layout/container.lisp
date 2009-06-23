;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Container layout

(def (component e) container/layout (component/layout contents/abstract frame-unique-id/mixin)
  ()
  (:documentation "A LAYOUT with several child COMPONENTs inside. The actual layout is set up on the remote side by style referring to its id."))

(def (macro e) container/layout ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'container/layout ,@args :contents (list ,@contents)))

(def (macro e) container ((&rest args &key &allow-other-keys) &body contents)
  `(container/layout ,args ,@contents))

(def render-component container/layout
  <div (:id ,(id-of -self-))
    ,(call-next-method)>)