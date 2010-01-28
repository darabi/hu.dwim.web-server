;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component action

(def (class* e) component-action (action)
  ((component :type component))
  (:metaclass funcallable-standard-class))

(def (macro e) make-component-action (component &body body)
  (with-unique-names (action)
    `(bind ((,action (make-instance 'component-action :component ,component)))
       (set-funcallable-instance-function ,action (named-lambda component-action-body ()
                                                    ,@body))
       ,action)))

(def method call-action :around (application session frame (action component-action))
  (with-restored-component-environment (component-of action)
    (call-next-method)))

;;;;;;
;;; Component rendering response

(def class* component-rendering-response (response)
  ((unique-counter 0 :type integer)
   (application :type application)
   (session :type session)
   (frame :type frame)
   (component :type component)))

;; TODO switch default content-type to +xhtml-mime-type+ (search for other uses, too)
;; seems like with xhtml there are random problems, like some dojo x.innerHTML throws...
(def (function e) make-component-rendering-response (component &key (application *application*) (session *session*) (frame *frame*)
                                                               (encoding +default-encoding+) (content-type (content-type-for +html-mime-type+ encoding)))
  (aprog1
      (make-instance 'component-rendering-response
                     :component component
                     :application application
                     :session session
                     :frame frame)
    (setf (header-value it +header/content-type+) content-type)))

(def (function e) make-root-component-rendering-response (frame &key (encoding +default-encoding+) (content-type (content-type-for +html-mime-type+ encoding)))
  (bind ((session (session-of frame))
         (application (application-of session)))
    (make-component-rendering-response (root-component-of frame)
                                       :application application
                                       :session session
                                       :frame frame
                                       :encoding encoding
                                       :content-type content-type)))

(def (function e) make-frame-root-component-rendering-response (frame-factory)
  (if *session*
      (if (root-component-of *frame*)
          (make-root-component-rendering-response *frame*)
          (progn
            (setf (root-component-of *frame*) (funcall frame-factory))
            (make-redirect-response-for-current-application)))
      (make-component-rendering-response (funcall frame-factory))))

(def method convert-to-primitive-response ((self component-rendering-response))
  (disallow-response-caching self)
  (bind ((*frame* (frame-of self))
         (*session* (session-of self))
         (*application* (application-of self))
         (*debug-component-hierarchy* (if *frame* (debug-component-hierarchy? *frame*) *debug-component-hierarchy*))
         (*ajax-aware-request* (ajax-aware-request?))
         (*delayed-content-request* (or *ajax-aware-request*
                                        (delayed-content-request?)))
         (body (with-output-to-sequence (buffer-stream :external-format (external-format-of self)
                                                       :initial-buffer-size 256)
                 (when (and *frame*
                            (not *delayed-content-request*))
                   (app.debug "This is not a delayed content request, clearing the action and client-state-sink hashtables of ~A" *frame*)
                   (clrhash (action-id->action-of *frame*))
                   (clrhash (client-state-sink-id->client-state-sink-of *frame*)))
                 (emit-into-xml-stream buffer-stream
                   (bind ((start-time (get-monotonic-time)))
                     (multiple-value-prog1
                         (call-in-rendering-environment *application* *session*
                                                        (lambda ()
                                                          (ajax-aware-render (component-of self))))
                       (app.info "Rendering done in ~,3f secs" (- (get-monotonic-time) start-time))))))))
    (app.debug "CONVERT-TO-PRIMITIVE-RESPONSE is returning a byte-vector-response of ~A bytes in the body" (length body))
    (make-byte-vector-response* body
                                :headers (headers-of self)
                                :cookies (cookies-of self))))

;;;;;;
;;; Ajax aware render

(def function ajax-aware-render (component)
  (assert (boundp '*rendering-phase-reached*))
  (app.debug "Inside AJAX-AWARE-RENDER; is this an ajax-aware-request? ~A" *ajax-aware-request*)
  (flet ((call-render-xhtml (component)
           (bind ((*inside-user-code* #t))
             (setf *rendering-phase-reached* #t)
             (app.debug "Rendering component ~A" component)
             (render-xhtml component))))
    (if (and *ajax-aware-request*
             (ajax-enabled? *application*))
        (bind ((to-be-rendered-components
                ;; KLUDGE: finding top/abstract and going down from there
                (bind ((top (find-descendant-component-with-type component 'top/abstract)))
                  (assert top nil "There is no TOP component below ~A, AJAX cannot be used in this situation at the moment" component)
                  (collect-covering-to-be-rendered-descendant-components top))))
          (setf (header-value *response* +header/content-type+) +xml-mime-type+)
          ;; FF does not like proper xml prologue, probably the other browsers even more so...
          ;; (emit-xml-prologue :encoding (guess-encoding-for-http-response) :stream *xml-stream* :version "1.1")
          <ajax-response
           ,@(with-collapsed-js-scripts
               (with-dojo-widget-collector
                 <dom-replacements (:xmlns #.+xml-namespace-uri/xhtml+)
                   ,(foreach (lambda (to-be-rendered-component)
                               (with-restored-component-environment (parent-component-of to-be-rendered-component)
                                 (call-render-xhtml to-be-rendered-component)))
                             to-be-rendered-components)>))
           <result "success">>)
        (call-render-xhtml component))))

(def function collect-covering-to-be-rendered-descendant-components (component)
  (prog1-bind covering-components nil
    (labels ((traverse (component)
               (catch component
                 (with-component-environment component
                   ;; NOTE: due to computed slots we must make sure that the component is refreshed, this might make the component to be rendered
                   (ensure-refreshed component)
                   (if (to-be-rendered-component? component)
                       (bind ((new-covering-component (find-ancestor-component-with-type component 'id/mixin)))
                         (assert new-covering-component nil "There is no covering ancestor component with id for ~A" component)
                         (let ((*print-level* 1))
                           (app.debug "Found to be rendered component ~A covered by component ~A" component new-covering-component))
                         (setf covering-components
                               (cons new-covering-component
                                     (remove-if (lambda (covering-component)
                                                  (find-ancestor-component covering-component [eq !1 new-covering-component]))
                                                covering-components)))
                         (throw new-covering-component nil))
                       (map-visible-child-components component #'traverse))))))
      (traverse component))))

(def method call-in-rendering-environment (application session thunk)
  (funcall thunk))
