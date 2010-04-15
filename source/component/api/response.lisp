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
                                                               (encoding (guess-encoding-for-http-response)) (content-type (content-type-for +html-mime-type+ encoding)))
  (aprog1
      (make-instance 'component-rendering-response
                     :component component
                     :application application
                     :session session
                     :frame frame)
    (setf (header-value it +header/content-type+) content-type)))

(def (function e) make-component-rendering-response/from-current-frame ()
  (assert (eq (session-of *frame*) *session*))
  (assert (eq (application-of *session*) *application*))
  (make-component-rendering-response (root-component-of *frame*)))

(def (function e) make-frame-root-component-rendering-response (&key
                                                                 content-component
                                                                 root-component
                                                                 (root-component-factory 'make-frame-root-component)
                                                                 (requires-valid-session #t)
                                                                 (requires-valid-frame requires-valid-session))
  (when (and requires-valid-session
             (not *session*))
    (error "~S requires a valid session" 'make-frame-root-component-rendering-response))
  (when (and requires-valid-frame
             (not *frame*))
    (error "~S requires a valid frame" 'make-frame-root-component-rendering-response))
  (flet ((compute-root-component ()
           (or root-component
               (and content-component
                    (funcall root-component-factory content-component))
               (funcall root-component-factory))))
    (if (and *session*
             *frame*)
        (progn
          (when (or root-component
                    content-component
                    (not (root-component-of *frame*)))
            (setf (root-component-of *frame*) (compute-root-component)))
          (make-component-rendering-response/from-current-frame))
        (make-component-rendering-response (compute-root-component)))))

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

(def function call-render-xhtml (component)
  (bind ((*inside-user-code* #t))
    (setf *rendering-phase-reached* #t)
    (app.debug "Rendering component ~A" component)
    (render-xhtml component)))

(def function ajax-aware-render (component)
  (assert (boundp '*rendering-phase-reached*))
  (app.debug "Inside AJAX-AWARE-RENDER; is this an ajax-aware-request? ~A" *ajax-aware-request*)
  (if (and *ajax-aware-request*
           (ajax-enabled? *application*))
      (incremental-render component)
      (full-render component)))

(def function full-render (component)
  "Renders COMPONENT fully without assuming any already existing state on the client side."
  (call-render-xhtml component))

(def function incremental-render (component)
  "Renders COMPONENT incrementally by sending the necessary state changes that needs to be applied on the client side.

There's an important invariant kept, namely calling FULL-RENDER at T0 (the initial timestamp), followed by multiple calls to INCREMENTAL-RENDER at subsequent Ti timestamps, is equivalent with calling FULL-RENDER at Tn (the last timestamp). Both of them will produce the same result on the client side if all requests and responses are properly processed by the server and the client. Between subsequent calls to INCREMENTAL-RENDER certain state changes are not allowed. TODO: er, which one?

Interesting use cases for INCREMENTAL-RENDER involves changing VISIBLE-COMPONENT?, TO-BE-RENDERED-COMPONENT?, LAZILY-RENDERED-COMPONENT? and not calling RENDER-COMPONENT on a child component."
  (bind ((to-be-rendered-components
          ;; KLUDGE: finding top/abstract and going down from there
          (bind ((top (find-descendant-component-with-type component 'top/abstract)))
            (assert top nil "There is no TOP component below ~A, AJAX cannot be used in this situation at the moment" component)
            (collect-covering-to-be-rendered-descendant-components top))))
    (setf (header-value *response* +header/content-type+) +xml-mime-type+)
    ;; FF does not like proper xml prologue, probably the other browsers even more so...
    ;; (emit-xml-prologue :encoding (guess-encoding-for-http-response) :stream *xml-stream* :version "1.1")
    <ajax-response
     ,@(with-xhtml-body-environment ()
         <dom-replacements (:xmlns #.+xml-namespace-uri/xhtml+)
          ,(foreach (lambda (to-be-rendered-component)
                      (map-descendant-components to-be-rendered-component [setf (rendered-component? !1) #f])
                      (with-restored-component-environment (parent-component-of to-be-rendered-component)
                        (call-render-xhtml to-be-rendered-component)))
                    to-be-rendered-components)>)
      <result "success">>))

#|
rendered visible to-be-rendered lazily-rendered what should we do in an incremental display?
#f       *       *              *               don't render
:stub    #f      *              *               don't render
:stub    #t      #f             *               don't render
:stub    #t      #t             #f              render and replace
:stub    #t      #t             #t              don't render
#t       #f      #f             #f              don't render and replace with stub
#t       #f      #t             #f              don't render and replace with stub
#t       #t      #f             #f              don't render
#t       #t      #t             #f              render and replace

what about a command-bar that is simply not rendered in its alternator and remains dirty?
e.g. render-component conditionally calls render-component on a child component
|#
(def function collect-covering-to-be-rendered-descendant-components (component)
  (prog1-bind covering-components nil
    (labels ((traverse (component)
               (catch component
                 (with-component-environment component
                   ;; NOTE: due to computed slots we must make sure that the component is refreshed, which might mark the component to-be-rendered
                   (ensure-refreshed component)
                   (bind ((rendered-component? (rendered-component? component))
                          (visible-component? (visible-component? component))
                          (to-be-rendered-component? (to-be-rendered-component? component))
                          (lazily-rendered-component? (lazily-rendered-component? component)))
                     (if (or (and (eq rendered-component? :stub)
                                  visible-component?
                                  to-be-rendered-component?
                                  (not lazily-rendered-component?))
                             (and (eq rendered-component? #t)
                                  (not visible-component?))
                             (and (eq rendered-component? #t)
                                  visible-component?
                                  to-be-rendered-component?))
                         ;; NOTE: stubs will be rendered by hideable/mixin automatically
                         (bind ((new-covering-component (find-ancestor-component component [and (typep !1 'id/mixin) (rendered-component? !1)]
                                                                                 :otherwise `(:error "Unable to find covering ancestor component (of type id/mixin) for ~A" ,component))))
                           (bind ((*print-level* 1))
                             (incremental.debug "Found to be rendered component ~A covered by component ~A (~A ~A ~A ~A)"
                                                component new-covering-component rendered-component? visible-component? to-be-rendered-component? lazily-rendered-component?))
                           (setf covering-components
                                 (cons new-covering-component
                                       (remove-if (lambda (covering-component)
                                                    (find-ancestor-component covering-component [eq !1 new-covering-component] :otherwise #f))
                                                  covering-components)))
                           (throw new-covering-component nil))
                         (unless (eq rendered-component? :stub)
                           (map-child-components component #'traverse))))))))
      (traverse component))))

(def method call-in-rendering-environment (application session thunk)
  (funcall thunk))
