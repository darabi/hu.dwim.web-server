(in-package :hu.dwim.wui)

(log.debug "Started evaluating wui.js")

(dojo.get-object "wui" #t)
(dojo.get-object "wui.io" #t)
(dojo.get-object "wui.i18n" #t)
(dojo.get-object "wui.field" #t)
(dojo.get-object "wui.help" #t)

(defun wui.shallow-copy (object)
  (return (dojo.mixin (create) object)))

(defun wui.append-query-parameter (url name value)
  (setf url (+ url
               (if (< (.index-of url "?") 0)
                   "?"
                   "&")
               (encodeURIComponent name)
               "="
               (encodeURIComponent value)))
  (return url))

(defun wui.decorate-url-with-frame-and-action (url (frame-id wui.frame-id) (frame-index wui.frame-index) action-id)
  (setf url (+ url (if (< (.index-of url "?") 0)
                       "?"
                       "&")))
  (setf url (+ url #.(escape-as-uri +frame-id-parameter-name+)    "=" (encodeURIComponent frame-id)
               "&" #.(escape-as-uri +frame-index-parameter-name+) "=" (encodeURIComponent frame-index)))
  (when action-id
    (setf url (+ url "&" #.(escape-as-uri +action-id-parameter-name+) "=" (encodeURIComponent action-id))))
  (return url))

(defun wui.absolute-url-from (url)
  (return
    (if (> (.index-of url ":") 0) ; KLUDGE this test is fragile here
        url
        (+ window.location.protocol "//"
           window.location.hostname ":"
           window.location.port
           url))))

(defun wui.communication-error (message)
  (setf this.message message))


;;;;;;
;;; io

(defun wui.io.action (url &key event (ajax true) (send-client-state true))
  (bind ((decorated-url (wui.append-query-parameter url
                                                    #.(escape-as-uri +ajax-aware-parameter-name+)
                                                    (if ajax "t" "")))
         (form (aref document.forms 0)))
    (wui.save-scroll-position "content")
    (if ajax
        (bind ((ajax-target (if (=== ajax true)
                                nil
                                (dojo.byId ajax))))
          (log.debug "Will fire an ajax request, ajax-target: " ajax-target)
          (bind ((ajax-request-in-progress-indicator (document.create-element "div"))
                 (ajax-request-in-progress-teardown (lambda ()
                                                      (when ajax-target
                                                        (ajax-request-in-progress-indicator.parent-node.remove-child ajax-request-in-progress-indicator)
                                                        (dojo.remove-class ajax-target "ajax-target"))))
                 (params (create :url decorated-url
                                 :form form
                                 :error (lambda (response io-args)
                                          (ajax-request-in-progress-teardown)
                                          (wui.io.process-ajax-error response io-args))
                                 :load (lambda (response io-args)
                                         (ajax-request-in-progress-teardown)
                                         (wui.io.process-ajax-answer response io-args)))))
            (when send-client-state
              (setf params.form form))
            (when ajax-target
              (dojo.add-class ajax-target "ajax-target")
              (dojo.add-class ajax-request-in-progress-indicator "ajax-request-in-progress")
              (dojo.content-box ajax-request-in-progress-indicator (dojo.content-box ajax-target))
              (dojo.place ajax-request-in-progress-indicator ajax-target "before"))
            (wui.io.xhr-post params)))
        (if (and send-client-state
                 form
                 (< 0 form.elements.length))
            (progn
              (setf (slot-value form 'action) decorated-url)
              (form.submit))
            (setf window.location.href decorated-url))))
  (when event
    (dojo.stop-event event)))

#+nil ;; TODO
(defun wui.io.eval-js-at-url (url error-handler)
  (ucw.io.bind (create :sync true
                       :url url
                       :session-id ucw.session-id
                       :frame-id ucw.frame-id
                       :load (lambda (type data event)
                               (log.debug "About to eval received script in eval-js-at-url")
                               (eval data))
                       :error error-handler
                       :mimetype "text/plain"
                       :method "get")))

#+nil ;; TODO
(defun wui.io.default-js-to-lisp-rpc-handler (type data event)
  (ucw.io.process-ajax-answer type data event)
  ;; TODO this with-stuff deserves a cleanup. it's only here to skip the body in case of an error...
  (with-ucw-error-handler
      (with-ajax-answer data
        (let ((return-value-node (aref (data.get-elements-by-tag-name "return-value") 0))
              (return-value-string return-value-node.firstChild))
          (log.debug "Return value (as string) is " return-value-string ", as node " return-value-node)
          (return (eval return-value-string))))))

(defun wui.io.postprocess-inserted-node (original-node imported-node)
  (log.debug "Parsing dojo widgets under " imported-node.id)
  ;; TODO check for widgets in ajax answers
  ;; (dojo.widget.create-widget imported-node)
  ;; seems like IE drops script nodes when setf'ing node.innerHTML, so let's walk the original-node
  ;; this must be another branidead try to make m$ crap secure...
  #+nil ;; TODO check the eval of toplevel script tags in ajax answers
  (cond (dojo.is-opera
         ;; TODO check other Opera versions, this is probably version dependent...
         (log.warn "NOT evaluating script tags, Opera does that automatically, version is " navigator.app-version))
        (t ;; (or dojo.is-mozilla dojo.isIE)
         (let ((script-nodes (if (= original-node.tag-name "script")
                                 (array original-node)
                                 (.get-elements-by-tag-name original-node "script"))))
           (.forEach script-nodes wui.io.eval-script-tag)))))

(defun wui.io.eval-script-tag (node)
  (let ((type (node.getAttribute "type")))
    (if (= type "text/javascript")
        (let ((script node.text))
          (unless (dojo.string.is-blank script)
            ;;(log.debug "Eval'ing script " (.substring script 0 128))
            (with-wui-error-handler
              (eval script))))
        (throw (+ "Script tag with unexpected type: '" type "'")))))

(defun wui.io.xhr-post (params)
  (setf params (wui.shallow-copy params))
  (macrolet ((default (name value)
               `(when (= (slot-value params ',name) undefined)
                  (setf (slot-value params ',name) ,value))))
    (default sync false) ;; TODO make true the default, and if true then find a way to numb event handlers meanwhile
    (default handle-as "xml")
    (default error wui.io.process-ajax-error)
    (default load wui.io.process-ajax-answer)

    ;; submit some forms as per caller request
    #+nil
    (when (and params.forms-to-submit
               (> params.forms-to-submit.length 0))
      (dolist (form params.forms-to-submit)
        (log.debug "Gathering values from form " form.id " as requested by :forms-to-submit")
        (dolist (field (wui.form.get-all-fields form))
          (let ((value (wui.field.get-value field))
                (name))
            (if (instanceof field dojo.widget.*widget)
                (macrolet ((wcase (&rest clauses)
                             `(cond ,@(iter (for entry :in clauses)
                                            (for test = (first entry))
                                            (for body = (rest entry))
                                            (collect `(,(if (eq test t)
                                                            t
                                                            `(and ,test
                                                                  (instanceof field ,test)))
                                                        ,@body))))))
                  (wcase (dojo.widget.*editor2
                          (setf name field.textarea.name))
                         (t
                          ;; the dojo guys say this is ok for any widget: http://trac.dojotoolkit.org/ticket/3283
                          (setf name field.name))))
                (setf name field.name))
            (log.debug "Value of " field ", named" name " is " value)
            (assert name)
            (setf (slot-value params.content name) value)))))

    (when (and params.url
               params.session-id
               params.frame-id
               params.action-id)
      (log.debug "Decorating wui.io.bind url with session, frame and action params. Before decoration the url is: " params.url)
      (setf params.url (wui.decorate-url-with-frame-and-action params.url
                                                               params.frame-id
                                                               params.frame-index
                                                               params.action-id)))

    ;; absolutize url if it's a relative one.
    (when params.url
      (setf params.url (wui.absolute-url-from params.url)))

    (let ((result (dojo.xhr-post params)))
      (return result))

    #+nil
    (let ((progress-label-remover (lambda ()
                                    (awhen params.progress-node
                                      (wui.io.progress.remove it)))))
      (wui.event.kw-connect (create
                             :src-obj params
                             :src-func (list "load" "error")
                             :advice-type "after"
                             :advice-func progress-label-remover))
      (wui.event.kw-connect (create
                             :src-obj params
                             :src-func "load"
                             :advice-type "after"
                             :advice-func wui.io.session-timeout-warning.notify-activity))
      (log.debug "Calling dojo.io.bind with " params)
      (let ((result (dojo.io.bind params)))
        (dojo.event.connect-before params "abort" (lambda ()
                                                    (progress-label-remover)
                                                    (setf wui.io.polling.enabled-p false)))
        (return result)))))

(defun wui.io.process-ajax-error (response io-args)
  (log.error "Processing AJAX error, name " response.name ", message: " response.message)
  (if dojo.config.isDebug
      debugger
      (window.location.reload)))

#+nil
(defun wui.io.execute-ajax-action (params)
  (with-wui-error-handler
    (macrolet ((only-one-of (primary secondary &key (defaulting T))
                 (let ((primary-name (CONCATENATE-STRING ":" (STRING-DOWNCASE (SYMBOL-NAME primary))))
                       (secondary-name (CONCATENATE-STRING ":" (STRING-DOWNCASE (SYMBOL-NAME secondary)))))
                   `(if (slot-value params ',primary)
                        (when (slot-value params ',secondary)
                          (log.debug "WARNING: ajax-action got params with both " ,primary-name " and "
                                     ,secondary-name "! Ignoring " ,secondary-name "..."))
                        ,(WHEN defaulting
                           `(setf (slot-value params ',primary) (slot-value params ',secondary))))))
               (default (name value)
                 `(unless (slot-value params ',name)
                    (setf (slot-value params ',name) ,value))))
      ;; do some sanity checks, defaulting and by-id lookups on the params
      (only-one-of load handler)
      (only-one-of error error-handler)
      (only-one-of mimetype mime-type)

      ;; TODO the extra args we are using (like handler) should be removed before calling dojo to avoid possible conflicts

      (log.debug "execute-ajax-action with params: " params)

      ;; TODO
      #+nil
      (unless (wui.may-abandon-the-page params.forms-to-ask params.forms-to-submit)
        (return false))

      (log.debug "Triggering AJAX action " params.url " with progress label " params.progress-label)

      ;; display the progress indicator
      ;; TODO for async requests, we could propagate params.abort to the progress code after bind was called and support aborting
      (when (not (= params.progress-label ""))
        (setf params.progress-node (wui.io.progress.display params.progress-label)))
      ;; let's fire the AJAX request! but delay it a bit, so the progress label gets a chance to display
      (window.set-timeout
       (lambda ()
         (let ((ok false))
           (unwind-protect
                (let ((result (wui.io.bind params)))
                  (setf ok true)
                  ;; FIXME this should return the value from execute-ajax-action, but dojo doesn't return it currently either
                  ;; TODO maybe this should throw a js exception? clean up js error handling
                  (return result))
             (unless ok
               ;; if some error happened before firing the request, so noone will call our error handler to remove it
               (awhen params.progress-node
                 (wui.io.progress.remove it))))))
       1))))

(defun wui.io.import-ajax-received-xhtml-node (node)
  ;; Makes an XMLHTTP-received node suitable for inclusion in the document.
  (log.debug "Importing ajax answer node with id " (.getAttribute node "id"))
  (cond
    (dojo.isMozilla
     (return node))
    ((or dojo.isOpera dojo.isSafari)
     (return (document.import-node node true)))
    (dojo.isIE
     ;; ie is randomly dropping the script tags (m$ is as lame as usual...)
     ;; i couldn't find anything that affects the behaviour, my best guess is that it may depend
     ;; on how the script reached the browser: scripts in the original document may have
     ;; more permissions? either way, handle script tags specially to overcome it.
     (let ((result)
           (copy-attributes (lambda (from to)
                              (dolist (attribute from.attributes)
                                (let ((value attribute.node-value))
                                  (setf (aref to attribute.node-name) value))))))
       (cond ((= node.tagName "script")
              (setf result (document.createElement "script"))
              (copy-attributes node result)
              (setf result.text node.text))
             ((= node.tagName "tr")
              ;; this is ugly here for a reason: ie sucks. seems like this is the only way to create a
              ;; tr node on the ie side that behaves as normal dom nodes (i.e. it can be added to the dom).
              ;; this was tested only on ie6.
              (setf result (document.createElement "tr"))
              (copy-attributes node result)
              (dolist (td node.childNodes)
                (if (= td.tagName "td")
                    (let ((body td.xml)
                          (start (1+ (.indexOf body ">")))
                          (end (- body.length 5))
                          (imported-td (document.createElement "td")))
                      ;; chop off the opening and the closing tag, so that we get the innerHTML
                      (setf body (.substring body start end))
                      (setf imported-td.innerHTML body)
                      (copy-attributes td imported-td)
                      (result.appendChild imported-td))
                    (result.appendChild (wui.io.import-ajax-received-xhtml-node td)))))
             (t
              ;; create a node and setf its innerXML property
              ;; this will parse the xhtml we received and convert it
              ;; to dom nodes that ie will not bark on.
              (setf result (document.createElement "div"))
              (log.debug "Assigning innerHTML")
              (setf result.innerHTML node.xml)
              (log.debug "innerHTML was assigned succesfully")
              (assert (= 1 result.childNodes.length))
              (setf result result.firstChild)))
       (log.debug "Succesfully imported answer node, returning")
       (return result))))
  (log.warn "Unknown browser in import-ajax-received-xhtml-node, this will probably cause some troubles later. Browser is " navigator.userAgent)
  (return node))

;; Return a lambda that when passed a root node, will call the visitor with each of those children
;; that have the given tag-name.
(defun wui.io.make-node-walker (tag-name visitor (import-node-p true) (toplevel-p false))
  (return
    (lambda (root)
      (dolist (toplevel-node root.child-nodes)
        (log.debug "Walking at node " toplevel-node.tag-name)
        ;; node.get-elements-by-tag-name returns recursively all nodes of a document node, so that won't work here
        (when (= toplevel-node.tag-name tag-name)
          (if toplevel-p
              (let ((node toplevel-node)
                    (original-node node)
                    (id (.getAttribute node "id")))
                (log.debug "Processing " tag-name " node with id " id)
                (when import-node-p
                  (setf node (wui.io.import-ajax-received-xhtml-node node)))
                (visitor node original-node))
              (progn
                (log.debug "Will process " toplevel-node.child-nodes.length " node(s) of type '" tag-name "'")
                (dolist (node (dojo._toArray toplevel-node.child-nodes)) ; create a copy and iterate on that
                  (when (slot-value node 'getAttribute)
                    (let ((original-node node)
                          (id (.getAttribute node "id")))
                      (log.debug "Processing " tag-name " node with id " id)
                      (when import-node-p
                        (setf node (wui.io.import-ajax-received-xhtml-node node)))
                      (visitor node original-node)))))))))))

;; Returns a lambda that can be used as a dojo :load handler.  Will do some sanity checks
;; on the ajax answer, report any possible server errors, then walk the nodes with the given
;; tag-name and call the visitor on them.  If the visitor returns a node, then postprocess the
;; returned node as an added dom html fragment.
(defun wui.io.make-node-walking-ajax-answer-processor (tag-name visitor (import-node-p true) (toplevel-p false))
  (let ((node-walker (wui.io.make-node-walker tag-name
                                              (lambda (node original-node)
                                                (when (visitor node original-node)
                                                  (log.debug "Calling postprocess-inserted-node on node " node)
                                                  (wui.io.postprocess-inserted-node original-node node)))
                                              import-node-p
                                              toplevel-p)))
    (return
      (lambda (response args)
        (log.debug "Response is " response)
        (log.debug "Args is " args)
        (with-ajax-answer response
          (node-walker response))))))

(bind ((dom-replacer
        (wui.io.make-node-walking-ajax-answer-processor "dom-replacements"
                                                        (lambda (replacement-node)
                                                          (bind ((id (.getAttribute replacement-node "id")))
                                                            (cond
                                                              ((and id ($ id))
                                                               (let ((old-node ($ id))
                                                                     (parent-node (slot-value old-node 'parent-node)))
                                                                 (hide-dom-node old-node)
                                                                 (log.debug "About to replace old node with id " id)
                                                                 (.replace-child parent-node replacement-node old-node)
                                                                 (log.debug "Successfully replaced node with id " id)
                                                                 (return true)))
                                                              ((= replacement-node.tagName "script")
                                                               (log.debug "Found a toplevel script node in dom-replacements, calling eval...")
                                                               (wui.io.eval-script-tag replacement-node))
                                                              (t (log.warn "Replacement node with id '" id "' was not found on the client side"))))))))
  (setf wui.io.process-ajax-answer
        (lambda (response args)
          (with-wui-error-handler
            ;; replace some components (dom nodes)
            (log.debug "Calling dom-replacer...")
            (dom-replacer response args)
            (log.debug "...dom-replacer returned")
            ;; look for 'script' tags and execute them with 'current-ajax-answer' bound
            (let ((script-evaluator
                   (wui.io.make-node-walking-ajax-answer-processor "script"
                                                                   (lambda (script-node)
                                                                     ;; TODO handle/assert for script type attribute
                                                                     (let ((script (dojox.xml.parser.textContent script-node)))
                                                                       (log.debug "About to eval AJAX-received script " #\Newline script)
                                                                       ;; isolate the local bindings from the script to be executed
                                                                       ;; and only bind with the given name what we explicitly list here
                                                                       ((lambda (_script current-ajax-answer)
                                                                          (eval _script)) script response)
                                                                       (log.debug "Finished eval-ing AJAX-received script")))
                                                                   false true)))
              (log.debug "Calling script-evaluator...")
              (script-evaluator response args)
              (log.debug "...script-evaluator returned"))))))

;;;;;;
;;; debug

(defun wui.reload-css ()
  (dolist (link (document.getElementsByTagName "link"))
    (when (and (>= (.indexOf (.toLowerCase link.rel) "stylesheet") 0)
               link.href)
      (bind ((href (.replace link.href (regexp "(&|\\?)forceReload=\\d+") "")))
        (setf link.href (wui.append-query-parameter href "forceReload" (.valueOf (new Date))))))))

;;;;;;
;;; scroll

(defun wui.reset-scroll-position ((content :by-id))
  (when content
    (bind ((form (aref document.forms 0))
           (sx (aref form #.+scroll-x-parameter-name+))
           (sy (aref form #.+scroll-y-parameter-name+)))
      (log.debug "Restoring scroll position: " sx.value sy.value)
      (setf content.scrollLeft sx.value)
      (setf content.scrollTop sy.value))))

(defun wui.save-scroll-position ((content :by-id))
  (when content
    (bind ((form (aref document.forms 0))
           (sx (aref form #.+scroll-x-parameter-name+))
           (sy (aref form #.+scroll-y-parameter-name+)))
      (log.debug "Saving scroll position: " content.scrollLeft content.scrollTop)
      (setf sx.value content.scrollLeft)
      (setf sy.value content.scrollTop))))

;;;;;;
;;; highlight

(defun wui.highlight-mouse-enter-handler (event (table :by-id) (row :by-id))
  (dojo.add-class row "highlighted")
  (let ((parent row.parent-node))
    (while (not (= parent document))
      (dojo.remove-class parent "highlighted")
      (setf parent parent.parent-node)))
  (dojo.stop-event event))

(defun wui.highlight-mouse-leave-handler (event (table :by-id) (row :by-id))
  (dojo.remove-class row "highlighted"))

;;;;;
;;; fields

;; TODO rename to wui.primitive.*?
(defun wui.field.setup-simple-checkbox (checkbox-id checked-tooltip unchecked-tooltip)
  (bind ((checkbox (dojo.byId checkbox-id))
         (hidden (dojo.byId (+ checkbox-id "_hidden"))))
    (log.debug "Setting up simple checkbox " checkbox ", using hidden input " hidden)
    (dojo.connect checkbox "onchange"
                  (lambda (event)
                    (let ((enabled checkbox.checked))
                      (log.debug "Propagating checkbox.checked of " checkbox " to the hidden field " hidden " named " hidden.name)
                      (setf hidden.value (if enabled
                                             "true"
                                             "false"))
                      (setf checkbox.title
                            (if enabled
                                checked-tooltip
                                unchecked-tooltip)))))
    (setf checkbox.wui-set-checked (lambda (enabled)
                                     (if (= checkbox.checked enabled)
                                         (return false)
                                         (progn
                                           (setf checkbox.checked enabled)
                                           ;; we need to be in sync, so call onchange explicitly
                                           (checkbox.onchange)
                                           (return true)))))
    (setf checkbox.wui-is-checked (lambda ()
                                    (return checkbox.checked)))))

(defun wui.field.setup-custom-checkbox (link-id checked-image unchecked-image checked-tooltip unchecked-tooltip checked-class unchecked-class)
  (bind ((link (dojo.byId link-id))
         (hidden (dojo.byId (+ link-id "_hidden"))))
    (log.debug "Setting up custom checkbox " link ", using hidden input " hidden)
    (bind ((image (aref (.get-elements-by-tag-name link "img") 0))
           (enabled (not (= hidden.value "false"))))
;; TODO:      (assert image)
      (if (and checked-image
               unchecked-image)
          (setf image.src (if enabled
                              checked-image
                              unchecked-image)))
      (setf link.className (if enabled
                               checked-class
                               unchecked-class))
      (setf link.title (if enabled
                           checked-tooltip
                           unchecked-tooltip)))
    (setf link.wui-set-checked (lambda (enabled)
                                 (setf hidden.value (if enabled
                                                        "true"
                                                        "false"))
                                 (if (and checked-image
                                          unchecked-image)
                                     (setf image.src (if enabled
                                                         checked-image
                                                         unchecked-image)))
                                 (setf link.className (if enabled
                                                          checked-class
                                                          unchecked-class))
                                 (setf link.title (if enabled
                                                      checked-tooltip
                                                      unchecked-tooltip))))
    (setf link.wui-is-checked (lambda ()
                                (return (not (= hidden.value "false")))))
    (setf link.name hidden.name)        ; copy name of the form input
    (setf link.onclick (lambda (event)
                         (link.wui-set-checked (not (link.wui-is-checked)))))))

(defun wui.field.update-popup-menu-select-field ((node :by-id) (field :by-id) value class)
  (if class
      (setf node.className class)
      (setf node.innerHTML value))
  (setf field.value value))

(defun wui.field.update-use-in-filter ((field :by-id) value)
  ;; TODO disable, or make transparent the other controls, too
  (field.wui-set-checked value))

(defun wui.field._setup-filter-field (widget-id use-in-filter-id)
  ;; for now it's shared between a few fields...
  (on-load
   (bind ((widget (dijit.byId widget-id))
          (listener (lambda ()
                      (wui.field.update-use-in-filter use-in-filter-id (!= "" (.getValue this))))))
     (assert widget)
     (widget.connect widget "onKeyUp" listener)
     (widget.connect widget "onChange" listener))))

(defun wui.field.setup-string-filter (widget-id use-in-filter-id)
  (wui.field._setup-filter-field widget-id use-in-filter-id))

(defun wui.field.setup-number-filter (widget-id use-in-filter-id)
  (wui.field._setup-filter-field widget-id use-in-filter-id))

;;;;;;
;;; compound

(setf wui.theme-setup-callbacks (create))

(defun wui.call-theme-setup-callbacks (type id &rest args &key &allow-other-keys)
  (bind ((callbacks (aref wui.theme-setup-callbacks type)))
    (when callbacks
      (dolist (callback callbacks)
        (callback id args)))))

(defun wui.register-theme-setup-callback (type fn)
  (bind ((callbacks (aref wui.theme-setup-callbacks type)))
    (unless callbacks
      (setf callbacks (array))
      (setf (aref wui.theme-setup-callbacks type) callbacks))
    (.push callbacks fn)))

(defun wui.setup-widget (type id &rest args &key &allow-other-keys)
  (wui.call-theme-setup-callbacks type id args))


;;;;;;
;;; i18n

(setf wui.i18n.resources (create))

(defun wui.i18n.localize (name)
  (let ((value (aref wui.i18n.resources name)))
    (unless value
      (log.error "Resource not found for key '" name "'")
      (setf value name))
    (return value)))

(defun wui.i18n.define ()
  (setf names-and-values arguments)
  (log.debug "Defining " names-and-values.length " i18n resources")
  (do ((idx 0 (+ idx 2)))
      ((>= idx names-and-values.length))
    (let ((name (aref names-and-values idx))
          (value (aref names-and-values (1+ idx))))
      (setf (aref wui.i18n.resources name) value))))

;;;;;;
;;; Context sensitive help

(setf wui.help.popup-timeout 400)
(setf wui.help.timer nil)

(defun wui.help.decorate-url (url id)
  (if (or (= id "")
          (= id undefined))
      (return url)
      (return (wui.append-query-parameter url #.(escape-as-uri +context-sensitive-help-parameter-name+) id))))

(defun wui.help.make-mouseover-handler (url)
  (return
    (lambda (event)
      (when wui.help.timer
        (clearTimeout wui.help.timer)
        (setf wui.help.timer nil))
      (setf wui.help.timer
            (setTimeout (lambda ()
                          (bind ((decorated-url url)
                                 (node event.target)
                                 (help wui.help.tooltip))
                            (while (not (= node document))
                              (setf decorated-url (wui.help.decorate-url decorated-url node.id))
                              (setf node node.parent-node))
                            (when (or (= help nil)
                                      (and help.has-loaded
                                           (not (= help.href decorated-url))))
                              (wui.help.teardown)
                              (setf help (new dojox.widget.DynamicTooltip
                                              (create :connectId (array event.target)
                                                      :position (array "below" "right")
                                                      :href decorated-url)))
                              (setf wui.help.tooltip help)
                              (help.open event.target))))
                        wui.help.popup-timeout))
      (dojo.stop-event event))))

(defun wui.help.setup (event url)
  (bind ((handles (array))
         (aborter (lambda (event)
                    (dojo.style document.body "cursor" "default")
                    (map 'dojo.disconnect handles)
                    (wui.help.teardown)
                    (dojo.stop-event event))))
    (handles.push (dojo.connect document "mouseover" (wui.help.make-mouseover-handler url)))
    (handles.push (dojo.connect document "click" aborter))
    (handles.push (dojo.connect document "keypress" (lambda (event)
                                                      (when (= event.charOrCode dojo.keys.ESCAPE)
                                                        (aborter event)))))
    (dojo.style document.body "cursor" "help")
    (dojo.stop-event event)))

(defun wui.help.teardown ()
  (when wui.help.tooltip
    (wui.help.tooltip.destroy)
    (setf wui.help.tooltip nil)
    (when wui.help.timer
      (clearTimeout wui.help.timer)
      (setf wui.help.timer nil))))




(log.debug "Finished evaluating wui.js")
