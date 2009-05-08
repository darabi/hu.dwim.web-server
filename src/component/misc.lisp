;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content

(def component content-component ()
  ((content nil :type component)))

(def render content-component
  (render (content-of -self-)))

(def refresh content-component
  (when-bind content (content-of -self-)
    (mark-outdated content)))

(def method component-value-of ((self content-component))
  (component-value-of (content-of self)))

(def method (setf component-value-of) (new-value (self content-component))
  (setf (component-value-of (content-of self)) new-value))

(def method find-command-bar ((component content-component))
  (or (call-next-method)
      (awhen (content-of component)
        (ensure-uptodate it)
        (find-command-bar it))))

;;;;;;
;;; Top

(def component top-component (remote-identity-component-mixin content-component recursion-point-component)
  ()
  (:documentation "The focus command will replace the content of the top-component with the component which the action refers to."))

(def render-xhtml top-component
  <div (:id ,(id-of -self-)) ,(call-next-method)>)

(def (macro e) top (&body content)
  `(make-instance 'top-component :content ,(the-only-element content)))

;;;;;;
;;; Empty

(eval-always
  (def component empty-component ()
    ()))

(def load-time-constant +empty-component-instance+ (make-instance 'empty-component))

(def render-xhtml empty-component
  (values))

(def (macro e) empty ()
  '+empty-component-instance+)

;;;;;;;
;;; Label

(def component label-component (style-component-mixin)
  ;; TODO rename the component-value slot
  ((component-value :type string)))

(def (macro e) label (text)
  `(make-instance 'label-component :component-value ,text))

(def render label-component
  (render (component-value-of -self-)))

;;;;;;
;;; Delay

(def component inline-component ()
  ((thunk)))

(def (macro e) inline-component (&body forms)
  `(make-instance 'inline-component :thunk (lambda () ,@forms)))

(def render-xhtml inline-component
  (funcall (thunk-of -self-)))

;;;;;;
;;; Wrapper

(def component wrapper-component ()
  ((thunk)
   (body)))

(def render-xhtml wrapper-component
  (bind (((:read-only-slots thunk body) -self-))
    (funcall thunk (lambda () (render body)))))

(def (macro e) make-wrapper-component (&rest args &key wrap &allow-other-keys)
  (remove-from-plistf args :wrap)
  `(make-instance 'wrapper-component
                  :thunk (lambda (body-thunk)
                           (flet ((body ()
                                    (funcall body-thunk)))
                             ,wrap))
                  ,@args))

;;;;;;
;;; Layered mixin

(def component layer-context-capturing-component-mixin ()
  ((layer-context (current-layer-context))))

(def component-environment layer-context-capturing-component-mixin
  (call-next-method)
  ;; TODO: revive layer capturing
  ;; FIXME: it overrides the rendering backend layer now
  #+nil(funcall-with-layer-context (layer-context-of -self-) #'call-next-method))

;;;;;;
;;; Remote identity mixin

(def component remote-identity-component-mixin ()
  ((id nil)))

(def render-xhtml :before remote-identity-component-mixin
  (when (not (id-of -self-))
    (setf (id-of -self-) (generate-response-unique-string "c"))))

(def function collect-covering-remote-identity-components-for-dirty-descendant-components (component)
  ;; KLUDGE: find top-component and go down from there to avoid
  (setf component (find-descendant-component-with-type component 'top-component))
  (assert component nil "There is no TOP-COMPONENT above ~A, AJAX cannot be used in this situation at the moment" component)
  (bind ((covering-components nil))
    (labels ((traverse (component)
               ;; NOTE: we cannot evaluate delayed visible here, because we are not in the component's environment
               (catch component
                 (with-component-environment component
                   (when (force (visible-p component))
                     (if (or (dirty-p component)
                             (outdated-p component))
                         (bind ((remote-identity-component (find-ancestor-component-with-type component 'remote-identity-component-mixin)))
                           (assert remote-identity-component nil "There is no ancestor component with remote identity for ~A" component)
                           (pushnew remote-identity-component covering-components)
                           (throw remote-identity-component nil))
                         (map-child-components component #'traverse)))))))
      (traverse component))
    (remove-if (lambda (component-to-be-removed)
                 (find-if (lambda (covering-component)
                            (and (not (eq component-to-be-removed covering-component))
                                 (find-ancestor-component component-to-be-removed [eq !1 covering-component])))
                          covering-components))
               covering-components)))

;;;;;;
;;; Style component mixin

(def component style-component-mixin ()
  ((id nil)
   (css-class nil)
   (style nil)))

(def render-xhtml style-component-mixin
  (bind (((:read-only-slots id css-class style) -self-))
    <div (:id ,id :class ,css-class :style ,style)
         ,(call-next-method) >))

;;;;;;
;;; Style component

(def component style-component (style-component-mixin content-component)
  ())

(def (macro e) style ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'style-component ,@args :content ,(the-only-element content)))

;;;;;;
;;; Initargs

(def component initargs-component-mixin ()
  ((initargs)))

(def method initialize-instance :after ((-self- initargs-component-mixin) &rest args &key &allow-other-keys)
  (setf (initargs-of -self-)
        ;; TODO: KLUDGE: dispatch on component for saved args?
        (iter (for arg :in '(:title :store-mode :page-size))
              (for value = (getf args arg :unbound))
              (unless (eq value :unbound)
                (collect arg)
                (collect value)))))

(def function inherited-initarg (component key)
  (awhen (find-ancestor-component-with-type component 'initargs-component-mixin)
    (bind ((value (getf (initargs-of it) key :unbound)))
      (if (eq value :unbound)
          (values nil #f)
          (values value #t)))))

;;;;;;
;;; Container

(def component container-component ()
  ((contents :type components)))

(def render-xhtml container-component
  (mapc #'render (contents-of -self-))
  nil)

(def (macro e) container (&body contents)
  `(make-instance 'container-component :contents (list ,@contents)))

;;;;;;
;;; Styled container

(def component styled-container-component (style-component-mixin container-component)
  ())

(def (macro e) styled-container ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'styled-container-component ,@args :contents (list ,@contents)))

;;;;;;
;;; Title component mixin

(def component title-component-mixin ()
  ((title :type component)))

(def (layered-function e) render-title (component)
  (:method :around ((component title-component-mixin))
    (bind ((id (generate-response-unique-string)))
      <span (:id ,id :class "title")
            ,(if (slot-boundp component 'title)
                 (when-bind title (force (title-of component))
                   `xml,title)
                 (call-next-method))>
      `js(wui.setup-widget "title" ,id)))

  (:method ((component title-component-mixin))
    (values)))

;;;;;;
;;; Recursion point

(def component recursion-point-component ()
  ())

(def (function e) find-ancestor-recursion-point-component (component)
  (find-ancestor-component-with-type (parent-component-of component) 'recursion-point-component))

;;;;;;
;;; Maker

(def component maker-component ()
  ()
  (:documentation "Abstract base class for all maker components."))

;;;;;;
;;; Inspector

(def component inspector-component ()
  ()
  (:documentation "Abstract base class for all inspector components."))

;;;;;;
;;; Filter

(def component filter-component ()
  ()
  (:documentation "Abstract base class for all filter components."))

;;;;;;
;;; Detail

(def component detail-component ()
  ()
  (:documentation "Abstract base class for components which show their component value in detail."))

;;;;;;
;;; tooltip

;; OPTIMIZATION: this could collect the essential data in a special variable and at the end of rendering emit a literal js array with all the tooltips
(def (function e) render-tooltip (connect-id tooltip &key position)
  ":position might be '(\"below\" \"right\")"
  (check-type tooltip (or string action function))
  (check-type connect-id string)
  (etypecase tooltip
    (string
     `js(on-load
         (new dijit.Tooltip
              (create :connectId (array ,connect-id)
                      :label ,tooltip
                      :position (array ,@position)))))
    ((or action uri)
     `js(on-load
         (new dojox.widget.DynamicTooltip
              (create :connectId (array ,connect-id)
                      :position (array ,@position)
                      :href ,(etypecase tooltip
                               (action (register-action/href tooltip :delayed-content #t))
                               (uri (print-uri-to-string tooltip)))))))
    ;; action is subtypep function, therefore this order and the small code duplication...
    (function
     `js(on-load
         (new dijit.Tooltip
              (create :connectId (array ,connect-id)
                      :label ,(babel:octets-to-string (with-output-to-sequence (*xml-stream* :external-format (external-format-of *response*))
                                                        (funcall tooltip))
                                                      :encoding (external-format-of *response*))
                      :position (array ,@position)))))))

;;;;;;
;;; Context sensitive help

(def (constant :test #'string=) +context-sensitive-help-parameter-name+ "_hlp")

(def component context-sensitive-help (content-component remote-identity-component-mixin)
  ((content (icon help) :type component)))

(def icon help :tooltip nil)

(def render-xhtml context-sensitive-help
  (when *frame*
    (bind ((href (register-action/href (make-action (execute-context-sensitive-help)) :delayed-content #t)))
      <div (:id ,(id-of -self-)
            :onclick `js-inline(wui.help.setup event ,href)
            :onmouseover `js-inline(bind ((kludge (wui.help.make-mouseover-handler ,href)))
                                     ;; KLUDGE for now cl-qq-js chokes on ((wui.help.make-mouseover-handler ,href))
                                     (kludge event)))
           ,(call-next-method)>)))

(def function execute-context-sensitive-help ()
  (with-request-params (((ids +context-sensitive-help-parameter-name+) nil))
    (setf ids (ensure-list ids))
    (bind ((components nil))
      (map-descendant-components (root-component-of *frame*)
                                 (lambda (descendant)
                                   (when (and (typep descendant 'remote-identity-component-mixin)
                                              (member (id-of descendant) ids :test #'string=))
                                     (push descendant components))))
      (make-component-rendering-response (make-instance 'context-sensitive-help-popup
                                                        :content (or (and components
                                                                          (make-context-sensitive-help (first components)))
                                                                     #"help.no-context-sensitive-help-available"))))))

(def (generic e) make-context-sensitive-help (component)
  (:method ((component component))
    nil)

  (:method :around ((component remote-identity-component-mixin))
    (or (call-next-method)
        (awhen (parent-component-of component)
          (make-context-sensitive-help it))))

  (:method ((self context-sensitive-help))
    #"help.help-about-context-sensitive-help-button"))

;;;;;;
;;; Help popup

(def component context-sensitive-help-popup (content-component)
  ())

(def render-xhtml context-sensitive-help-popup
  <div (:class "context-sensitive-help-popup") ,(call-next-method)>)

;;;;;;
;;; Center tag wrapper

(def component center-tag-wrapper-mixin ()
  ())

(def render-xhtml :around center-tag-wrapper-mixin
  <center ,(call-next-method)>)
