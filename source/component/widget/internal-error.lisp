;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; internal-error-message/widget

(def (component e) internal-error-message/widget (panel/widget)
  ((rendering-phase-reached :type boolean)
   (error :type serious-condition)
   (original-root-component)))

(def refresh-component internal-error-message/widget
  (bind (((:slots title-bar) -self-))
    (setf title-bar (title/widget ()
                      #"error.internal-server-error.title"))))

(def layered-method make-command-bar-commands ((self internal-error-message/widget) class prototype value)
  (bind (((:read-only-slots rendering-phase-reached original-root-component) self))
    (list* (make-instance 'command/widget
                          :content (icon/widget navigate-back)
                          :action (if rendering-phase-reached
                                      (make-uri-for-new-frame)
                                      (make-action
                                        (setf (root-component-of *frame*) original-root-component))))
           (call-next-method))))

(def method handle-toplevel-error/application/emit-response ((application application) (error serious-condition) (ajax-aware? (eql #f)))
  (bind ((*response* nil)) ; avoid an assert from firing. is this a KLUDGE?
    (bind ((request-uri (raw-uri-of *request*)))
      (app.info "Sending an internal server error page for request ~S coming to application ~A" request-uri application)
      (bind ((rendering-phase-reached *rendering-phase-reached*)
             (component (make-frame-root-component
                         (make-instance 'internal-error-message/widget
                                        :rendering-phase-reached rendering-phase-reached
                                        :error error
                                        :original-root-component (when *frame*
                                                                   (root-component-of *frame*))
                                        :content (inline-render-component/widget ()
                                                   ;; TODO split the content of render-application-internal-error-page into separate l10n entries and drop the call to apply-localization-function
                                                   ;; TODO don't use component-message/widget here
                                                   (render-component (component-message/widget (:category :error)
                                                                       #"error.internal-server-error"))
                                                   (apply-localization-function 'render-application-internal-error-page
                                                                                (list :administrator-email-address (administrator-email-address-of application))))))))
        (bind ((response (if *frame*
                             (progn
                               (setf (root-component-of *frame*) component)
                               (make-root-component-rendering-response *frame*))
                             (make-component-rendering-response component))))
          (unwind-protect
               (send-response response)
            (close-response response)))))))
