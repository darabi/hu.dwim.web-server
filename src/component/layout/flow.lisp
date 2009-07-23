;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; flow/layout

(def (component e) flow/layout (layout/minimal contents/abstract)
  ((orientation :horizontal :type (member :vertical :horizontal))
   (direction :forward :type (member :forward :backward))))

(def (macro e) flow/layout ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'flow/layout ,@args :contents (list ,@contents)))

(def render-xhtml flow/layout
  ;; TODO: orientation, direction
  <div (:class "flow layout" :style "float: left;")
    ,(render-contents-for -self-)>)
