;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

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

(def class* component-rendering-response (locked-session-response-mixin)
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

(def method convert-to-primitive-response ((self component-rendering-response))
  (disallow-response-caching self)
  (bind ((*frame* (frame-of self))
         (*session* (session-of self))
         (*application* (application-of self))
         (*debug-component-hierarchy* (if *frame* (debug-component-hierarchy-p *frame*) *debug-component-hierarchy*))
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
  (app.debug "Inside AJAX-AWARE-RENDER; is this an ajax-aware-request? ~A" *ajax-aware-request*)
  (if (and *ajax-aware-request*
           (ajax-enabled? *application*))
      (bind ((dirty-components
              ;; KLUDGE: finding top/abstract and going down from there
              (bind ((top (find-descendant-component-with-type component 'top/abstract)))
                (assert top nil "There is no TOP component below ~A, AJAX cannot be used in this situation at the moment" component)
                (collect-covering-id-components-for-descendant-components top #'to-be-rendered-component?))))
        (setf (header-value *response* +header/content-type+) +xml-mime-type+)
        ;; FF does not like proper xml prologue, probably the other browsers even more so...
        ;; (emit-xml-prologue)
        <ajax-response
         ,@(with-collapsed-js-scripts
             (with-dojo-widget-collector
               <dom-replacements (:xmlns #.+xml-namespace-uri/xhtml+)
                 ,(foreach (lambda (dirty-component)
                             (with-restored-component-environment (parent-component-of dirty-component)
                               (bind ((*inside-user-code* #t))
                                 (setf *rendering-phase-reached* #t)
                                 (render-xhtml dirty-component))))
                           dirty-components)>))
         <result "success">>)
      (bind ((*inside-user-code* #t))
        (setf *rendering-phase-reached* #t)
        (render-xhtml component))))

(def function collect-covering-id-components-for-descendant-components (component predicate)
  (bind ((covering-components nil))
    (labels ((traverse (component)
               (catch component
                 (with-component-environment component
                   (when (visible-component? component)
                     (if (funcall predicate component)
                         (bind ((ancestor (find-ancestor-component-with-type component 'id/mixin)))
                           (assert ancestor nil "There is no ancestor component with id for ~A" component)
                           (pushnew ancestor covering-components)
                           (throw ancestor nil))
                         (map-child-components component #'traverse)))))))
      (traverse component))
    (remove-if (lambda (component-to-be-removed)
                 (find-if (lambda (covering-component)
                            (and (not (eq component-to-be-removed covering-component))
                                 (find-ancestor-component component-to-be-removed [eq !1 covering-component])))
                          covering-components))
               covering-components)))

(def method call-in-rendering-environment (application session thunk)
  (funcall thunk))