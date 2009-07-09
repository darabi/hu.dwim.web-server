;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Debug client side

(def special-variable *debug-client-side* (not *load-as-production-p*)
  "The default, system wide, value for the debug-client-side slots of frames.")

(def generic compile-time-debug-client-side? (application))

;;;;;;
;;; Logging

(macrolet ((forward (name)
             `(def js-macro ,(symbolicate "log." name) (&rest args)
                (when *debug-client-side*
                 (list* ',(symbolicate "window.console." name) args)))))
  (forward |debug|)
  (forward |info|)
  (forward |warn|)
  (forward |error|)
  (forward |critical|))

;;;;;;
;;; Lift some definitions to JavaScript

(def (js-macro e) |in-package| (package)
  (declare (ignore package))
  (values))

(def (js-macro e) |bind| (args &body body)
  {with-preserved-readtable-case
   `(let ,ARGS ,@BODY)})

(def (js-macro e) |on-load| (&body body)
  {with-preserved-readtable-case
   `(dojo.add-on-load
     (lambda ()
       ,@BODY))})

(def (js-macro e) |with-dom-nodes| (bindings &body body)
  (iter (for binding :in bindings)
        (bind (((variable-name tag-name &key ((:|class| class))) binding))
          (collect {with-preserved-readtable-case
                     `(,VARIABLE-NAME (document.createElement ,TAG-NAME))} :into node-bindings)
          (when class
            (collect {with-preserved-readtable-case
                       `(dojo.addClass ,VARIABLE-NAME ,CLASS)} :into forms)))
        (finally (return {with-preserved-readtable-case
                           `(bind (,@NODE-BINDINGS)
                              ,@FORMS
                              ,@BODY)}))))

(def (js-macro e) |create-dom-node| (name)
  {with-preserved-readtable-case
    `(document.createElement ,NAME)})

(def (js-macro e) |hide-dom-node| (node)
  {with-preserved-readtable-case
   `(setf (slot-value (slot-value ,NODE 'style) 'display) "none")})

(def (js-macro e) |show-dom-node| (node)
  {with-preserved-readtable-case
   `(setf (slot-value (slot-value ,NODE 'style) 'display) "")})

(def (js-macro e) |get-first-child-with-tag-name| (node tag-name)
  {with-preserved-readtable-case
   `(aref (.get-elements-by-tag-name ,NODE ,TAG-NAME) 0)})

(def (js-macro e) |when-bind| (var condition &body body)
  {with-preserved-readtable-case
   `(let ((,VAR ,CONDITION))
      (if ,VAR
          (progn
            ,@BODY)))})

(def (js-macro e) |awhen| (condition &body body)
  {with-preserved-readtable-case
    `(when-bind it ,CONDITION
       ,@BODY)})

(def (js-macro e) |aif| (condition then else)
  {with-preserved-readtable-case
   `(let ((it ,CONDITION))
      (if it
          ,THEN
          ,ELSE))})

(def (js-macro e) $ (&body things)
  (if (length= 1 things)
      {with-preserved-readtable-case
       `(dojo.by-id ,(FIRST THINGS))}
      {with-preserved-readtable-case
       `(map 'dojo.by-id ,THINGS)}))

(def js-macro |defun| (name args &body body)
  (bind ((name-pieces (cl-ppcre:split "\\." (symbol-name name)))
         (arg-names (iter (for arg :in args)
                          (collect (if (listp arg)
                                       (car arg)
                                       arg))))
         (arg-processors (iter (for arg :in args)
                               (when (listp arg)
                                 (unless (= (length arg) 2)
                                   (error "Hm, what do you mean by ~S?" arg))
                                 (let ((name (first arg)))
                                   (case (second arg)
                                     (:|by-id|
                                      (collect `{with-preserved-readtable-case
                                                 (setf ,NAME ($ ,NAME))}))
                                     (:|widget-by-id|
                                      (collect `{with-preserved-readtable-case
                                                 (setf ,NAME (dojo.widget.by-id ,NAME))}))
                                     (t
                                      (collect `{with-preserved-readtable-case
                                                 (when (=== ,NAME undefined)
                                                   (setf ,NAME ,(CADR ARG)))}))))))))
    (if (length= 1 name-pieces)
        {with-preserved-readtable-case
         `(CL-QUASI-QUOTE-JS:defun ,NAME ,ARG-NAMES
            ,@ARG-PROCESSORS
            ,@BODY)}
        {with-preserved-readtable-case
         `(progn
            (setf ,NAME (lambda ,ARG-NAMES
                          ,@ARG-PROCESSORS
                          ,@BODY)))})))

(def (js-macro e) |with-wui-error-handler| (&body body)
  {with-preserved-readtable-case
    `(try
          (progn
            ,@BODY)
       (catch (e)
         (if (= e "graceful-abort")
             (log.debug "Gracefully aborting execution and returning to toplevel")
             (progn
               (log.warn "Exception reached toplevel: " e)
               (if dojo.config.isDebug
                   debugger
                   (let ((message (wui.i18n.localize "unknown-error-at-toplevel")))
                     (alert message)))))))})

(def (js-macro e) |with-ajax-answer| (data &body body)
  (with-unique-js-names (result-node)
    {with-preserved-readtable-case
      `(progn
         (log.info "Processing AJAX answer")
         (setf ,DATA (get-first-child-with-tag-name ,DATA "ajax-response"))
         (unless ,DATA
           (log.warn "AJAX ajax-response node is nil, probably a malformed response, maybe a full page load due to an unregistered action id?")
           (throw (new wui.communication-error "AJAX answer is empty")))
         (log.debug "Found ajax-response DOM node")
         (let ((,RESULT-NODE (get-first-child-with-tag-name ,DATA "result")))
           (if (or (not ,RESULT-NODE)
                   (not (= (dojo.string.trim (dojox.xml.parser.textContent ,RESULT-NODE))
                           "success")))
               (let ((error-message (wui.i18n.localize "unknown-server-error")))
                 (when-bind error-node (get-first-child-with-tag-name ,DATA "error-message")
                   (setf error-message (dojox.xml.parser.textContent error-node)))
                 (when-bind error-handler-node (get-first-child-with-tag-name ,DATA "error-handler")
                   (eval (dojox.xml.parser.textContent error-handler-node))
                   (throw "graceful-abort"))
                 (when error-message
                   (alert error-message))
                 (throw "graceful-abort"))
               ,@BODY)))}))

(def (js-macro e) |assert| (expression &rest args-to-throw)
  (unless args-to-throw
    (setf args-to-throw (list (concatenate 'string "Assertion failed: " (princ-to-string expression)))))
  (bind ((enter-debugger? *debug-client-side*))
   {with-preserved-readtable-case
       `(unless ,EXPRESSION
          ,@(WHEN ENTER-DEBUGGER?
             '(debugger))
          (throw ,@ARGS-TO-THROW))}))
