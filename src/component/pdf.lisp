;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Definer

(def (layered-function e) render-pdf (component))

(def (definer e) render-pdf (&body forms)
  (bind ((layer (when (member (first forms) '(:in-layer :in))
                  (pop forms)
                  (pop forms)))
         (qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms)))
    `(def layered-method render-pdf ,@(when layer `(:in ,layer)) ,@(when qualifier (list qualifier)) ((-self- ,type))
       ,@forms)))

;;;;;;
;;; Command

(def icon export-pdf "static/wui/icons/20x20/pdf-document.png")
(def resources hu
  (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "A tartalom mentése PDF formátumban"))
(def resources en
  (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "Export content in PDF format"))

(def function make-export-pdf-command (component)
  (command (icon export-pdf)
           (make-action
             (make-raw-functional-response ((:content-type +pdf-mime-type+))
               (emit-http-response ()
                 (render-pdf component))))))

;;;;;;
;;; Render

;; TODO: move to components
(def render-pdf table-component ()
  )

(def render-pdf column-component ()
  )

(def render-pdf row-component ()
  )

(def render-pdf cell-component ()
  )
