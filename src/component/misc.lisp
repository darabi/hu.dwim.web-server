;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Empty component

(eval-always
  (def component empty-component ()
    ()
    (:documentation "A completely empty component. This is used as a singleton instead of NIL, which is not a valid component.")))

(def load-time-constant +empty-component-instance+ (make-instance 'empty-component))

(def render empty-component
  (values))

(def (macro e) empty ()
  '+empty-component-instance+)

;;;;;;
;;; Value mixin

(def component value-mixin ()
  ((component-value :computed-in compute-as :type t))
  (:documentation "A component that shows a value"))

;;;;;;
;;; Border mixin

(def component border-mixin ()
  ()
  (:documentation "A component with a border"))

;;;;;;
;;; Top mixin

(def component top-mixin ()
  ()
  (:documentation "The focus command will replace the content of the top component with the component referred by the action"))

;;;;;;
;;; Top component

(def component top-component (top-mixin content-mixin remote-identity-mixin recursion-point-mixin user-messages-mixin)
  ())

(def (macro e) top (&body content)
  `(make-instance 'top-component :content ,(the-only-element content)))

(def render-xhtml top-component
  <div (:id ,(id-of -self-))
    ,(render-user-messages -self-)
    ,(call-next-method)>)

(def (function e) find-top-component-content (component)
  (awhen (find-ancestor-component-with-type component 'top-mixin)
    (content-of it)))

(def (function e) top-component-p (component)
  (eq component (find-top-component-content component)))

;;;;;;;
;;; Label component

(def component label-component (value-mixin style-mixin)
  ((component-value :type string)))

(def (macro e) label ((&key id css-class style) text)
  `(make-instance 'label-component :id ,id :css-class ,css-class :style ,style :component-value ,text))

(def render label-component
  (render (component-value-of -self-)))

;;;;;;
;;; Inline component

(def component inline-component ()
  ((thunk)))

(def render inline-component
  (funcall (thunk-of -self-)))

(def (macro e) inline-component (&body forms)
  `(make-instance 'inline-component :thunk (lambda () ,@forms)))

(def method component-value-of ((self inline-component))
  nil)

;;;;;;
;;; Wrapper component

(def component wrapper-component ()
  ((thunk)
   (body)))

(def render wrapper-component
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
;;; Layer context capturing mixin

(def component layer-context-capturing-mixin ()
  ((layer-context (current-layer-context))))

(def component-environment layer-context-capturing-mixin
  (call-next-method)
  ;; TODO: revive layer capturing
  ;; FIXME: it overrides the rendering backend layer now
  #+nil(funcall-with-layer-context (layer-context-of -self-) #'call-next-method))

;;;;;;
;;; Initargs mixin

(def component initargs-mixin ()
  ((initargs)))

(def method initialize-instance :after ((-self- initargs-mixin) &rest args &key &allow-other-keys)
  (setf (initargs-of -self-)
        ;; TODO: KLUDGE: dispatch on component for saved args?
        (iter (for arg :in '(:title :store-mode :page-size))
              (for value = (getf args arg :unbound))
              (unless (eq value :unbound)
                (collect arg)
                (collect value)))))

(def function inherited-initarg (component key)
  (awhen (find-ancestor-component-with-type component 'initargs-mixin)
    (bind ((value (getf (initargs-of it) key :unbound)))
      (if (eq value :unbound)
          (values nil #f)
          (values value #t)))))

;;;;;;
;;; Container component

(def component container-component ()
  ((contents :type components)))

(def render container-component
  (foreach #'render (contents-of -self-)))

(def (macro e) container (&body contents)
  `(make-instance 'container-component :contents (list ,@contents)))

;;;;;;
;;; Styled container component

(def component styled-container-component (style-mixin container-component)
  ())

(def (macro e) styled-container ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'styled-container-component ,@args :contents (list ,@contents)))

;;;;;;
;;; Recursion point mixin

(def component recursion-point-mixin ()
  ())

(def (function e) find-ancestor-recursion-point (component)
  (find-ancestor-component-with-type (parent-component-of component) 'recursion-point-mixin))

;;;;;;
;;; Maker component

(def component maker-component ()
  ()
  (:documentation "Abstract base class for all maker components."))

;;;;;;
;;; Inspector component

(def component inspector-component ()
  ()
  (:documentation "Abstract base class for all inspector components."))

;;;;;;
;;; Filter component

(def component filter-component ()
  ()
  (:documentation "Abstract base class for all filter components."))

;;;;;;
;;; Detail component

(def component detail-component ()
  ()
  (:documentation "Abstract base class for components which show their component value in detail."))

;;;;;;
;;; Tooltip

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
