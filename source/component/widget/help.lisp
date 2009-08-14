;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Context sensitive help

(def (constant :test #'string=) +context-sensitive-help-parameter-name+ "_hlp")

(def (icon e) help :tooltip nil)

(def (component e) context-sensitive-help (content/mixin id/mixin)
  ()
  (:default-initargs :content (icon help)))

(def render-xhtml context-sensitive-help
  (when *frame*
    (bind ((href (register-action/href (make-action (show-context-sensitive-help -self-)) :delayed-content #t)))
      <div (:id ,(id-of -self-)
            :onclick `js-inline(wui.help.setup event ,href)
            :onmouseover `js-inline(bind ((kludge (wui.help.make-mouseover-handler ,href)))
                                     ;; KLUDGE for now cl-qq-js chokes on ((wui.help.make-mouseover-handler ,href))
                                     (kludge event)))
           ,(call-next-method)>)))

(def layered-function show-context-sensitive-help (component)
  (:method ((self context-sensitive-help))
    (with-request-params (((ids +context-sensitive-help-parameter-name+) nil))
      (setf ids (ensure-list ids))
      (bind ((components nil))
        (map-descendant-components (root-component-of *frame*)
                                   (lambda (descendant)
                                     (when (and (typep descendant 'id/mixin)
                                                (member (id-of descendant) ids :test #'string=))
                                       (push descendant components))))
        (make-component-rendering-response (make-instance 'context-sensitive-help-popup
                                                          :content (or (and components
                                                                            (make-context-sensitive-help (first components)))
                                                                       #"help.no-context-sensitive-help-available")))))))

(def (generic e) make-context-sensitive-help (component)
  (:method ((component component))
    nil)

  (:method :around ((component id/mixin))
    (or (call-next-method)
        (awhen (parent-component-of component)
          (make-context-sensitive-help it))))

  (:method ((self context-sensitive-help))
    #"help.help-about-context-sensitive-help-button"))

;;;;;;
;;; Context sensitive help popup

(def (component e) context-sensitive-help-popup (content/mixin)
  ())

(def render-xhtml context-sensitive-help-popup
  <div (:class "context-sensitive-help-popup")
       ,(call-next-method)>)
