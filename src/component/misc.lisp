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
;;; Layered mixin

(def component layered-component-mixin ()
  ((layer-context (current-layer-context))))

(def call-in-component-environment layered-component-mixin ()
  (funcall-with-layer-context (layer-context-of -self-) #'call-next-method))

;;;;;;
;;; Remote identity mixin

(def component remote-identity-component-mixin ()
  ((id nil)))

(def render :before remote-identity-component-mixin ()
  (with-slots (id) -self-
    (when (and *frame*
               (not id))
      (setf id (generate-frame-unique-string "c")))))

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
;;; Detail

(def component detail-component ()
  ()
  (:documentation "Abstract base class for components which show their component value in detail."))


;;;;;;
;;; File download

(def component file-download-component (command-component)
  ((icon (icon download))
   (action nil)
   (directory)
   (file-name)
   (url-prefix "static/")))

(def constructor (file-download-component (label nil label?) &allow-other-keys) ()
  (when label?
    (setf (icon-of -self-) (icon download :label label))))

(def method refresh-component ((self file-download-component))
  (with-slots (file-name action url-prefix) self
    (unless action
      (setf action
            (make-uri :path (concatenate 'string url-prefix (namestring file-name)))))))

(def render file-download-component ()
  (with-slots (icon file-name action url-prefix directory enabled) -self-
    (bind ((absolute-file-name (merge-pathnames file-name directory)))
      (unless (probe-file absolute-file-name)
        (setf enabled #f))
      <div (:class "file-download")
           ,(call-next-method)
           " (" ,(file-last-modification-timestamp absolute-file-name) ")">)))

(defresources en
  (file-last-modification-timestamp (file)
    `xml,"Updated: "
    (if (probe-file file)
        (localized-timestamp (local-time:universal-to-timestamp (file-write-date file)))
        <span (:class "missing-file")
              "File is missing!">)))

(defresources hu
  (file-last-modification-timestamp (file)
    `xml,"Frissítve: "
    (if (probe-file file)
        (localized-timestamp (local-time:universal-to-timestamp (file-write-date file)))
        <span (:class "missing-file")
              "Hiányzik a fájl!">)))

;;;;;;
;;; File upload

(def component file-upload-component ()
  ((icon (icon upload) :type component)))

(def render file-upload-component ()
  (with-slots (icon) -self-
    <div ,(render icon)
         <div <input (:type "file")>>>))

;;;;;;
;;; Frame

(def component frame-component (top-component layered-component-mixin)
  ((content-type +xhtml-content-type+)
   (stylesheet-uris nil)
   (script-uris nil)
   (page-icon nil)
   (title nil)
   (dojo-skin-name "tundra")
   (dojo-path "static/dojo/dojo/")
   (dojo-file-name "dojo.js")
   (parse-dojo-widgets-on-load #f :type boolean :accessor parse-dojo-widgets-on-load?)
   (debug-client-side (not *load-as-production-p*) :type boolean :accessor debug-client-side? :export :accessor)))

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
             (with-dojo-widget-collector
               (render (content-of -self-))))>>>))

(def (macro e) frame ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'frame-component ,@args :content ,(the-only-element content)))
