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
  (find-command-bar (content-of component)))

;;;;;;
;;; Top

(def component top-component (content-component)
  ()
  (:documentation "The top command will replace the content of a top-component with the component which the action refers to."))

;;;;;;
;;; Empty

(def component empty-component ()
  ())

(def render empty-component ()
  +void+)

;;;;;;;
;;; Label

(def component label-component ()
  ((component-value)))

(def render label-component ()
  <span ,(component-value-of -self-)>)

;;;;;;
;;; Delay

(def component inline-component ()
  ((thunk)))

(def render inline-component ()
  (funcall (thunk-of -self-)))

(def (macro e) make-inline-component (&body forms)
  `(make-instance 'inline-component :thunk (lambda () ,@forms)))

;;;;;;
;;; Wrapper

(def component wrapper-component ()
  ((thunk)
   (body)))

(def render wrapper-component ()
  (with-slots (thunk body) -self-
    (funcall thunk (lambda () (render body)))))

(def (macro e) make-wrapper-component (&rest args &key wrap &allow-other-keys)
  (remove-from-plistf args :wrap)
  `(make-instance 'wrapper-component
                  :thunk (lambda (body-thunk)
                           (flet ((body ()
                                    (funcall body-thunk)))
                             ,wrap))
                  ,@args))

;;;;;;;;;
;;; Remote identity

(def component remote-identity-component-mixin (component)
  ((id nil)))

(def method id-of :around ((self remote-identity-component-mixin))
  (bind ((id (call-next-method)))
    (unless id
      (setf id (generate-frame-unique-string "c"))
      (setf (id-of self) id))
    id))

;;;;;;;;;
;;; Widget

(def component widget-component-mixin (remote-identity-component-mixin)
  ((css-class nil :initarg :class)
   (style nil)))

(def component widget-component (widget-component-mixin)
  ((child :type component)))

(def render widget-component ()
  <div (:id ,(id-of -self-) :class ,(css-class-of -self-) :style ,(style-of -self-))
       ,(render (child-of -self-))>)

;;;;;;
;;; Detail

(def component detail-component ()
  ())

;;;;;;
;;; Value

(def component value-component ()
  ())

(def constructor value-component ()
  (setf (component-value-of -self-) (component-value-of -self-)))

;;;;;;;;;
;;; Frame

(def component frame-component (top-component)
  ((content-type +xhtml-content-type+)
   (stylesheet-uris nil)
   (page-icon nil)
   (title nil)
   (dojo-skin-name "tundra")
   (dojo-path "static/dojo/dojo/")
   (dojo-file-name "dojo.js")
   (parse-dojo-widgets-on-load #f :type boolean :accessor parse-dojo-widgets-on-load?)
   (debug-client-side #f :type boolean :accessor debug-client-side?)
   (%cached-dojo-uri nil)))

(def render frame-component ()
  (unless (%cached-dojo-uri-of -self-)
    (setf (%cached-dojo-uri-of -self-) (concatenate-string (path-prefix-of *application*)
                                                           (dojo-path-of -self-)
                                                           (dojo-file-name-of -self-))))
  (bind ((response (when (boundp '*response*)
                     *response*))
         (encoding (or (when response
                         (encoding-name-of response))
                       +encoding+)))
    (emit-xhtml-prologue encoding +xhtml-1.1-doctype+)
    <html (:xmlns     #.+xhtml-namespace-uri+
           xmlns:dojo #.+dojo-namespace-uri+)
      <head
        <meta (:http-equiv #.+header/content-type+ :content ,(content-type-for +html-mime-type+ encoding))>
        ,(awhen (page-icon-of -self-)
           <link (:rel "icon" :type "image/x-icon" :href ,(ensure-uri-string it))>)
        <title ,(title-of -self-)>
        ,@(mapcar (lambda (stylesheet-uri)
                    <link (:rel "stylesheet" :type "text/css"
                                :href ,(ensure-uri-string stylesheet-uri))>)
                  (stylesheet-uris-of -self-))
        <script (:type         #.+javascript-mime-type+
                 :src          ,(%cached-dojo-uri-of -self-)
                 :djConfig     ,(format nil "parseOnLoad: ~A, isDebug: ~A"
                                        (to-js-boolean (parse-dojo-widgets-on-load? -self-))
                                        (to-js-boolean (debug-client-side? -self-))))
                 ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                 "">>
      <body (:class ,(dojo-skin-name-of -self-))
        <form (:method "post")
          ,@(with-collapsed-js-scripts
             <span
              ;; TODO move into a standalone js broker
              `js(defun submit-form (href)
                   (let ((form (aref (slot-value document 'forms) 0)))
                     (setf (slot-value form 'action) href)
                     (form.submit)))>
             (render (content-of -self-)))>>>))
