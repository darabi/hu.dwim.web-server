;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Context sensitive help

(def (constant :test #'string=) +context-sensitive-help-parameter-name+ "_hlp")

(def icon help :tooltip nil)

(def component context-sensitive-help (content-mixin
                                       remote-identity-mixin)
  ()
  (:default-initargs :content (icon help)))

(def render-xhtml context-sensitive-help
  (when *frame*
    (bind ((href (register-action/href (make-action (execute-context-sensitive-help -self-)) :delayed-content #t)))
      <div (:id ,(id-of -self-)
            :onclick `js-inline(wui.help.setup event ,href)
            :onmouseover `js-inline(bind ((kludge (wui.help.make-mouseover-handler ,href)))
                                     ;; KLUDGE for now cl-qq-js chokes on ((wui.help.make-mouseover-handler ,href))
                                     (kludge event)))
           ,(call-next-method)>)))

(def layered-function execute-context-sensitive-help (component)
  (:method ((self context-sensitive-help))
    (with-request-params (((ids +context-sensitive-help-parameter-name+) nil))
      (setf ids (ensure-list ids))
      (bind ((components nil))
        (map-descendant-components (root-component-of *frame*)
                                   (lambda (descendant)
                                     (when (and (typep descendant 'remote-identity-mixin)
                                                (member (id-of descendant) ids :test #'string=))
                                       (push descendant components))))
        (make-component-rendering-response (make-instance 'context-sensitive-help-popup
                                                          :content (or (and components
                                                                            (make-context-sensitive-help (first components)))
                                                                       #"help.no-context-sensitive-help-available")))))))

(def (generic e) make-context-sensitive-help (component)
  (:method ((component component))
    nil)

  (:method :around ((component remote-identity-mixin))
    (or (call-next-method)
        (awhen (parent-component-of component)
          (make-context-sensitive-help it))))

  (:method ((self context-sensitive-help))
    #"help.help-about-context-sensitive-help-button"))

;;;;;;
;;; Context sensitive help popup

(def component context-sensitive-help-popup (content-mixin)
  ())

(def render-xhtml context-sensitive-help-popup
  <div (:class "context-sensitive-help-popup")
       ,(call-next-method)>)
