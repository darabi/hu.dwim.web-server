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
        (find-command-bar it))))

;;;;;;
;;; Top

(def component top-component (content-component)
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

;;;;;;
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

(def component style-component-mixin (remote-identity-component-mixin)
  ((css-class nil)
   (style nil)))

(def component style-component (style-component-mixin content-component)
  ())

(def render style-component-mixin ()
  <div (:id ,(id-of -self-) :class ,(css-class-of -self-) :style ,(style-of -self-))
       ,(call-next-method) >)

(def (macro e) style ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'style-component ,@args :content ,(the-only-element content)))

;;;;;;
;;; Container

(def component container-component ()
  ((contents :type components)))

(def render container-component ()
  <div ,@(mapcar #'render (contents-of -self-))>)

(def (macro e) container (&body contents)
  `(make-instance 'container-component :contents (list ,@contents)))

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
   (script-uris nil)
   (page-icon nil)
   (title nil)
   (dojo-skin-name "tundra")
   (dojo-path "static/dojo/dojo/")
   (dojo-file-name "dojo.js")
   (parse-dojo-widgets-on-load #f :type boolean :accessor parse-dojo-widgets-on-load?)
   (debug-client-side #f :type boolean :accessor debug-client-side?)))

(def render frame-component ()
  (bind ((application *application*)
         (path-prefix (path-prefix-of application))
         (response (when (boundp '*response*)
                     *response*))
         (encoding (or (when response
                         (encoding-name-of response))
                       +encoding+))
         (debug-client-side? (debug-client-side? -self-)))
    (emit-xhtml-prologue encoding +xhtml-1.1-doctype+)
    <html (:xmlns     #.+xhtml-namespace-uri+
           xmlns:dojo #.+dojo-namespace-uri+)
      <head
        <meta (:http-equiv #.+header/content-type+
               :content ,(content-type-for +html-mime-type+ encoding))>
        ,(awhen (page-icon-of -self-)
           <link (:rel "icon"
                  :type "image/x-icon"
                  :href ,(concatenate-string path-prefix
                                             (etypecase it
                                               (string it)
                                               (uri (print-uri-to-string it)))))>)
        <title ,(title-of -self-)>
        ,@(mapcar (lambda (stylesheet-uri)
                    <link (:rel "stylesheet"
                           :type "text/css"
                           :href ,(concatenate-string path-prefix (etypecase stylesheet-uri
                                                                    (string stylesheet-uri)
                                                                    (uri (print-uri-to-string stylesheet-uri)))))>)
                  (stylesheet-uris-of -self-))
        <script (:type         #.+javascript-mime-type+
                 :src          ,(concatenate-string path-prefix
                                                    (dojo-path-of -self-)
                                                    (dojo-file-name-of -self-)
                                                    (when debug-client-side?
                                                      ".uncompressed.js"))
                 :djConfig     ,(format nil "parseOnLoad: ~A, isDebug: ~A, locale: ~A"
                                        (to-js-boolean (parse-dojo-widgets-on-load? -self-))
                                        (to-js-boolean debug-client-side?)
                                        (to-js-literal (default-locale-of application))))
                 ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                 "">
        ,@(mapcar (lambda (script-uri)
                    <script (:type         #.+javascript-mime-type+
                             :src          ,(concatenate-string path-prefix script-uri))
                            ;; it must have an empty body because browsers don't like collapsed <script ... /> in the head
                            "">)
                  (script-uris-of -self-))>
      <body (:class ,(dojo-skin-name-of -self-))
        <form (:method "post")
          ,@(with-collapsed-js-scripts
             <span
              ;; TODO move into a standalone js broker
              `js(defun submit-form (href)
                   (let ((form (aref (slot-value document 'forms) 0)))
                     (setf (slot-value form 'action) href)
                     (form.submit)))>
             (with-dojo-widget-collector
               (render (content-of -self-))))>>>))

(def (macro e) frame ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'frame-component ,@args :content ,(the-only-element content)))
