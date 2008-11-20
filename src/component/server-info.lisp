;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Frame size component

;; TODO maybe this should be done through a customized factory method? so that inspecting server instances bring up the customized view...
(def component server-info ()
  ((server *server*)
   (server-inspector :type component)
   (application-inspectors :type component)
   (command-bar :type component)))

(def constructor server-info
  (setf (command-bar-of -self-) (command-bar
                                  (make-refresh-command -self-))))

(def method refresh-component ((self server-info))
  (setf (server-inspector-of self) (make-viewer (server-of self))))

(def render server-info ()
  <div (:class "server-info")
    ,(render (server-inspector-of -self-))
    ,(render (command-bar-of -self-))>)
