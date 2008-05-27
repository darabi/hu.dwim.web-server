;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content

(def component content-component ()
  ((content :type component)))

(def render content-component ()
  (render (content-of self)))

;;;;;;
;;; Top

(def component top-component (content-component)
  ())

;;;;;;
;;; Frame

(def component frame-component (top-component)
  ((stylesheets)
   (title)))

(def render frame-component ()
  (with-slots (stylesheets title content) self
    <html
     <head
      <meta (:http-equiv "Content-Type" :content "text/html; charset=utf-8")>
      <title ,title>
      ,@(mapcar (lambda (stylesheet)
                  <link (:rel "stylesheet" :type "text/css" :href ,stylesheet)>)
                stylesheets)>
     <body
      ;; KLUDGE: kill this hack and make state synchronization transparent
      <form (:action "/" :method "post")
            ,(render content)
            <input (:type submit :value "Synchronize")>>>>))

;;;;;;
;;; Empty

(def component empty-component ()
  ())

(def render empty-component ()
  +void+)

;;;;;;
;;; Delay

;; TODO: this probably has to be deleted, I don't like the way it works
(def component delay-component ()
  ((thunk)))

(def render delay-component ()
  (with-slots (thunk) self
    (render (funcall thunk))))

;;;;;;
;;; Wrapper

(def component wrapper-component ()
  ((thunk)
   (body)))

(def render wrapper-component ()
  (with-slots (thunk body) self
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
;;; Detail

(def component detail-component ()
  ())
