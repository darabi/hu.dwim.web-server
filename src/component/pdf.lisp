;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Command

(def icon export-pdf "static/wui/icons/20x20/pdf-document.png")

(def resources hu
    (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "A tartalom mentése PDF formátumban"))

(def resources en
    (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "Export content in PDF format"))

(def special-variable *pdf-stream*)

(def function make-export-pdf-command (component)
  (command (icon export-pdf)
           (make-action
             (make-raw-functional-response ((+header/content-type+ +pdf-mime-type+))
               (send-headers *response*)
               (bind ((*pdf-stream* (network-stream-of *request*)))
                 (execute-export-pdf component))))))

(def layered-function execute-export-pdf (component)
  (:method ((component component))
    (render-pdf component)))

;;;;;;
;;; Render
