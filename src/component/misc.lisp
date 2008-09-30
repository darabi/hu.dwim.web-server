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

(def method find-command-bar ((component content-component))
  (or (call-next-method)
      (awhen (content-of component)
        (ensure-uptodate it)
        (find-command-bar it))))

;;;;;;
;;; Top

(def component top-component (remote-identity-component-mixin content-component)
  ()
  (:documentation "The top command will replace the content of a top-component with the component which the action refers to."))

(def (macro e) top (&body content)
  `(make-instance 'top-component :content ,(the-only-element content)))

;;;;;;
;;; Empty

(def component empty-component ()
  ())

(def render empty-component ()
  +void+)

(def (macro e) empty ()
  '(make-instance 'empty-component))

;;;;;;;
;;; Label

(def component label-component ()
  ;; TODO rename the component-value slot
  ((component-value)))

(def (macro e) label (text)
  `(make-instance 'label-component :component-value ,text))

(def render label-component ()
  <span ,(component-value-of -self-)>)

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

;; TODO rename to layer-context-capturing-component-mixin
(def component layered-component-mixin ()
  ((layer-context (current-layer-context))))

(def call-in-component-environment layered-component-mixin ()
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
  (prog1-bind covering-components nil
    (labels ((traverse-1 (parent-component)
               (if (typep parent-component 'remote-identity-component-mixin)
                   (catch parent-component
                     (traverse-2 parent-component))
                   (traverse-2 parent-component)))
             (traverse-2 (parent-component)
               ;; TODO: typep instead of find-slot
               (when (force (visible-p parent-component))
                 (if (dirty-p parent-component)
                     (bind ((remote-identity-component
                             (if (typep parent-component 'remote-identity-component-mixin)
                                 parent-component
                                 (find-ancestor-component-with-type parent-component 'remote-identity-component-mixin))))
                       (assert remote-identity-component nil "There is no ancestor component with remote identity for ~A" parent-component)
                       (pushnew remote-identity-component covering-components)
                       (throw remote-identity-component nil))
                     (map-child-components parent-component #'traverse-1)))))
      (traverse-1 component))))

;;;;;;;;;
;;; Style

(def component style-component-mixin ()
  ((id nil)
   (style nil)
   (css-class nil)))

(def render style-component-mixin ()
  <div (:id ,(id-of -self-) :class ,(css-class-of -self-) :style ,(style-of -self-))
       ,(call-next-method) >)

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
        ;; TODO: dispatch on component for saved args?
        (iter (for arg :in '(:store-mode))
              (for value = (getf args arg :unbound))
              (unless (eq value :unbound)
                (collect arg)
                (collect value)))))

;;;;;;
;;; Container

(def component container-component ()
  ((contents :type components)))

(def render container-component ()
  <div ,@(mapcar #'render (contents-of -self-))>)

(def (macro e) container (&body contents)
  `(make-instance 'container-component :contents (list ,@contents)))

;;;;;;
;;; Styled container

(def component styled-container-component (style-component-mixin container-component)
  ())

(def (macro e) styled-container ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'styled-container-component ,@args :contents (list ,@contents)))

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
