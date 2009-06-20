;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tooltip mixin

(def (component e) tooltip/mixin ()
  ((tooltip :type component))
  (:documentation "A COMPONENT with a tooltip."))

;;;;;;
;;; Tooltip basic

(def (component e) tooltip/basic (content/mixin)
  ())

(def render-component tooltip/basic
  (render-tooltip -self-))

;; OPTIMIZATION: this could collect the essential data in a special variable and at the end of rendering emit a literal js array with all the tooltips
(def (function e) render-tooltip (connect-id tooltip &key position)
  ":position might be '(\"below\" \"right\")"
  (check-type tooltip (or string uri action function))
  (check-type connect-id string)
  (etypecase tooltip
    (string
     `js(on-load
         (new dijit.Tooltip
              (create :connectId (array ,connect-id)
                      :label ,tooltip
                      :position (array ,@position)))))
    ((or action uri)
     `js(on-load
         (new dojox.widget.DynamicTooltip
              (create :connectId (array ,connect-id)
                      :position (array ,@position)
                      :href ,(etypecase tooltip
                               (action (register-action/href tooltip :delayed-content #t))
                               (uri (print-uri-to-string tooltip)))))))
    ;; action is subtypep function, therefore this order and the small code duplication...
    (function
     `js(on-load
         (new dijit.Tooltip
              (create :connectId (array ,connect-id)
                      :label ,(babel:octets-to-string (with-output-to-sequence (*xml-stream* :external-format (external-format-of *response*))
                                                        (funcall tooltip))
                                                      :encoding (external-format-of *response*))
                      :position (array ,@position)))))))

;;;;;;
;;; Tooltip full

(def (component e) tooltip/full (tooltip/basic style/mixin remote-setup/mixin)
  ())
