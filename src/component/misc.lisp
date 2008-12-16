;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content

(def component content-component ()
  ((content nil :type component)))

(def render content-component ()
  (render (content-of -self-)))

(def render-csv content-component ()
  (render-csv (content-of -self-)))

(def render-pdf content-component ()
  (render-pdf (content-of -self-)))

(def method refresh-component ((self content-component))
  (when-bind content (content-of self)
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
  (:documentation "The top command will replace the content of a top-component with the component which the action refers to."))

(def render top-component ()
  <div (:id ,(id-of -self-)) ,(call-next-method)>)

(def (macro e) top (&body content)
  `(make-instance 'top-component :content ,(the-only-element content)))

;;;;;;
;;; Empty

(eval-always
  (def component empty-component ()
    ()))

(def load-time-constant +empty-component-instance+ (make-instance 'empty-component))

(def render empty-component ()
  +void+)

(def (macro e) empty ()
  '+empty-component-instance+)


;;;;;;;
;;; Label

(def component label-component ()
  ;; TODO rename the component-value slot
  ((component-value)))

(def (macro e) label (text)
  `(make-instance 'label-component :component-value ,text))

(def render label-component ()
  <span ,(component-value-of -self-)>)

(def render-csv label-component ()
  (render-csv (component-value-of -self-)))

(def render-pdf label-component ()
  (render-pdf (component-value-of -self-)))

;;;;;;
;;; Delay

(def component inline-component ()
  ((thunk)))

(def (macro e) inline-component (&body forms)
  `(make-instance 'inline-component :thunk (lambda () ,@forms)))

(def render inline-component ()
  (funcall (thunk-of -self-)))

;;;;;;
;;; Wrapper

(def component wrapper-component ()
  ((thunk)
   (body)))

(def render wrapper-component ()
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

(def call-in-component-environment layer-context-capturing-component-mixin ()
  (funcall-with-layer-context (layer-context-of -self-) #'call-next-method))

;;;;;;
;;; Remote identity mixin

(def component remote-identity-component-mixin ()
  ((id nil)))

(def render :before remote-identity-component-mixin
  (when (and *frame*
             (not (id-of -self-)))
    (setf (id-of -self-) (generate-frame-unique-string "c"))))

(def function collect-covering-remote-identity-components-for-dirty-descendant-components (component)
  ;; KLUDGE: find top-component and go down from there to avoid
  (setf component (find-descendant-component-with-type component 'top-component))
  (bind ((covering-components nil))
    (labels ((traverse (component)
               (when (force (visible-p component))
                 (if (or (dirty-p component)
                         (outdated-p component))
                     (bind ((remote-identity-component
                             (find-ancestor-component-with-type component 'remote-identity-component-mixin)))
                       (assert remote-identity-component nil "There is no ancestor component with remote identity for ~A" component)
                       (pushnew remote-identity-component covering-components))
                     (map-child-components component #'traverse)))))
      (traverse component))
    (remove-if (lambda (component-to-be-removed)
                 (find-if (lambda (covering-component)
                            (and (not (eq component-to-be-removed covering-component))
                                 (find-ancestor-component component-to-be-removed [eq !1 covering-component])))
                          covering-components))
               covering-components)))

;;;;;;;;;
;;; Style

(def component style-component-mixin ()
  ((id nil)
   (css-class nil)
   (style nil)))

(def render style-component-mixin ()
  (bind (((:read-only-slots id css-class style) -self-))
    <div (:id ,id :class ,css-class :style ,style)
         ,(call-next-method) >))

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
        (iter (for arg :in '(:title :store-mode :page-count))
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

(def render container-component ()
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

(def method refresh-component :before ((self title-component-mixin))
  (unless (slot-boundp self 'title)
    (bind (((:values title provided?) (inherited-initarg self :title)))
      (when provided?
        (setf (title-of self) title)))))

;;;;;;
;;; Recursion point

(def component recursion-point-component ()
  ())

;;;;;;
;;; Maker

(def component maker-component ()
  ()
  (:documentation "Base class for all maker components."))

;;;;;;
;;; Inspector

(def component inspector-component ()
  ()
  (:documentation "Base class for all inspector components."))

;;;;;;
;;; Filter

(def component filter-component ()
  ()
  (:documentation "Base class for all filter components."))

;;;;;;
;;; Detail

(def component detail-component ()
  ()
  (:documentation "Abstract base class for components which show their component value in detail."))

;;;;;;
;;; Context sensitive help

(def (constant :test #'string=) +context-sensitive-help-parameter-name+ "_hlp")

(def component context-sensitive-help (content-component)
  ((content (icon help) :type component)))

(def icon help)
(def resources hu
  (icon-label.help "Segítség")
  (icon-tooltip.help "Környezetfüggő segítség megjelenítése"))
(def resources en
  (icon-label.login "Help")
  (icon-tooltip.login "Display context sensitive help"))

(def render context-sensitive-help ()
  (when *frame*
    (bind ((href (register-action/href (make-action (execute-context-sensitive-help)) :delayed-content #t)))
      <div (:onclick `js-inline(wui.setup-context-sensitive-help event ,href)) ,(call-next-method)>)))

(def function execute-context-sensitive-help ()
  (bind ((ids (ensure-list (request-parameter-value *request* +context-sensitive-help-parameter-name+)))
         (components nil))
    (map-descendant-components (root-component-of *frame*)
                               (lambda (descendant)
                                 (when (and (typep descendant 'remote-identity-component-mixin)
                                            (member (id-of descendant) ids :test #'string=))
                                   (push descendant components))))
    (aif (and components
              (make-context-sensitive-help (first components)))
         (make-component-rendering-response it)
         (make-component-rendering-response #"help.no-context-sensitive-help-available"))))

(def generic make-context-sensitive-help (component)
  (:method ((component remote-identity-component-mixin))
    nil
    ;; TODO: kill, used for debugging
    #+nil
    (string-downcase (class-name (class-of component)))))

(def resources hu
  (help.no-context-sensitive-help-available "Nincs környezetfüggő segítség"))

(def resources en
  (help.no-context-sensitive-help-available "No conext sensitive help available"))
