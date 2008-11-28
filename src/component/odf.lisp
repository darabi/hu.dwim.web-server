;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Command

(def icon export-odf "static/wui/icons/20x20/document.png")

(def resources hu
    (icon-label.export-odf "ODF")
  (icon-tooltip.export-odf "A tartalom mentése ODF formátumban"))

(def resources en
    (icon-label.export-odf "ODF")
  (icon-tooltip.export-odf "Export content in ODF format"))

(def special-variable *odf-stream*)

(def (function e) make-export-odf-command (component)
  (command (icon export-odf)
           (make-action
             (make-raw-functional-response ((+header/content-type+ +odf-mime-type+))
               (send-headers *response*)
               (bind ((*odf-stream* (network-stream-of *request*)))
                 (execute-export-odf component))))))

(def layered-function execute-export-odf (component)
  (:method ((component component))
    (render-odf component)))

;;;;;;
;;; Render
