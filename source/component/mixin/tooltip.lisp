;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tooltip/mixin

(def (component e) tooltip/mixin ()
  ((tooltip nil :type (or null component)))
  (:documentation "A COMPONENT with a tooltip."))

(def (function e) render-tooltip-for (component)
  (awhen (tooltip-of component)
    (render-tooltip it (id-of component))))

;; TODO: this could collect the essential data in a special variable and at the end of rendering emit a literal js array with all the tooltips
(def (function e) render-tooltip (tooltip target-id &key position)
  ":position might be '(\"below\" \"right\")"
  (check-type tooltip (or string uri action function))
  (check-type target-id string)
  (etypecase tooltip
    (string
     ;; alternative onhover: (dijit.showTooltip ,tooltip ,target-id (array ,@position))
     `js-onload(new dijit.Tooltip
                    (create :connectId (array ,target-id)
                            :label ,tooltip
                            :position (array ,@position))))
    ((or action uri)
     `js-onload(new dojox.widget.DynamicTooltip
                    (create :connectId (array ,target-id)
                            :position (array ,@position)
                            :href ,(etypecase tooltip
                                              (action (register-action/href tooltip :delayed-content #t))
                                              (uri (print-uri-to-string tooltip))))))
    ;; action is subtypep function, therefore this order and the small code duplication...
    (computation
     `js-onload(new dijit.Tooltip
                    (create :connectId (array ,target-id)
                            :label ,(babel:octets-to-string (emit-into-xml-stream-buffer (:external-format (external-format-of *response*))
                                                              (force tooltip))
                                                            :encoding (external-format-of *response*))
                            :position (array ,@position))))))
