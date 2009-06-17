;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon abstract

(def (component ea) icon/abstract ()
  ())

(def method supports-debug-component-hierarchy? ((self icon/abstract))
  #f)

;;;;;;
;;; Icon basic

(def (component ea) icon/basic (icon/abstract tooltip/mixin)
  ((name :type symbol)
   (label :type (or null string))
   (image-path :type string)))

(def render icon/basic
  (render-component (label-of -self-)))

(def render-xhtml icon/basic
  (render-icon :icon -self-))

(def layered-function render-icon-label (icon label)
  (:method (icon label)
    `xml,label))

(def (function e) render-icon (&key icon (name nil name?) (label nil label?) (image-path nil image-path?) (tooltip nil tooltip?) (class nil class?))
  (when (and icon
             (not (stringp icon)))
    (unless name?
      (setf name (name-of icon)))
    (unless label?
      (setf label (label-of icon)))
    (unless image-path?
      (setf image-path (image-path-of icon)))
    (unless tooltip?
      (setf tooltip (tooltip-of icon))))
  (bind ((tooltip (force tooltip))
         (id (generate-response-unique-string))
         (class (if class?
                    class
                    (icon-class name))))
    ;; render the `js first, so the return value contract of qq is kept.
    (when tooltip
      (render-tooltip id tooltip))
    <span (:id ,id
           :class ,class)
      ,(when image-path
         <img (:src ,(concatenate-string (path-prefix-of *application*) image-path))>)
      ,(awhen (force label)
         (render-icon-label icon it))>))

(def function icon-class (name)
  (concatenate-string "icon " (string-downcase (symbol-name name)) "-icon"))

(def (macro e) icon (name &rest args)
  `(make-icon/basic ',name ,@args))

(def (function e) make-icon/basic (name &rest args)
  (bind ((icon (find-icon name :otherwise nil)))
    (if icon
        (apply #'make-instance 'icon/basic
               :name name (append args
                                  (list :label (label-of icon)
                                        :image-path (image-path-of icon)
                                        :tooltip (tooltip-of icon))))
        (if args
            (apply #'make-instance 'icon/basic :name name args)
            (error "The icon ~A cannot be found and no arguments were specified" name)))))

(def special-variable *icons* (make-hash-table))

(def (function e) find-icon (name &key (otherwise ;; TODO breaks sbcl, bug reported `(:error "The icon ~A cannot be found" ,name)
                                                  (list :error "The icon ~A cannot be found" name)))
  (prog1-bind icon (gethash name *icons*)
    (unless icon
      (handle-otherwise otherwise))))

(def function (setf find-icon) (icon name)
  (setf (gethash name *icons*) icon))

(def (definer e) icon (name &key image-path (label nil label-p) (tooltip nil tooltip-p))
  (bind ((name-as-string (string-downcase name)))
    `(setf (find-icon ',name)
           (make-instance 'icon/basic
                          :name ',name
                          :image-path ,image-path
                          :label ,(if label-p
                                      label
                                      `(delay (lookup-resource ,(concatenate-string "icon-label." name-as-string))))
                          :tooltip ,(if tooltip-p
                                        tooltip
                                        `(delay (lookup-resource ,(concatenate-string "icon-tooltip." name-as-string))))))))

;;;;;;
;;; Default icons
;;; TODO: move the icons where they are actually used

(def icon new)
(def resources hu
  (icon-label.new "Új")
  (icon-tooltip.new "Új objektum szerkesztése"))
(def resources en
  (icon-label.new "New")
  (icon-tooltip.new "Start editing a new object"))

(def icon create)
(def resources hu
  (icon-label.create "Létrehozás")
  (icon-tooltip.create "Új objektum felvétele"))
(def resources en
  (icon-label.create "Create")
  (icon-tooltip.create "Create object"))

(def icon delete)
(def resources hu
  (icon-label.delete "Törlés")
  (icon-tooltip.delete "Az objektum törlése"))
(def resources en
  (icon-label.delete "Delete")
  (icon-tooltip.delete "Delete object"))

(def icon close)
(def resources hu
  (icon-label.close "Bezárás")
  (icon-tooltip.close "A komponens bezárása"))
(def resources en
  (icon-label.close "Close")
  (icon-tooltip.close "Close the component"))

(def icon back)
(def resources hu
  (icon-label.back "Vissza")
  (icon-tooltip.back "Vissza a helyére"))
(def resources en
  (icon-label.back "Back")
  (icon-tooltip.back "Move back"))

(def icon expand)
(def resources hu
  (icon-label.expand "Kinyitás")
  (icon-tooltip.expand "Részletek megjelenítése"))
(def resources en
  (icon-label.expand "Expand")
  (icon-tooltip.expand "Expand to detail"))

(def icon collapse)
(def resources hu
  (icon-label.collapse "Összecsukás")
  (icon-tooltip.collapse "Részletek elrejtése"))
(def resources en
  (icon-label.collapse "Collapse")
  (icon-tooltip.collapse "Collapse to reference"))

(def icon filter)
(def resources hu
  (icon-label.filter "Keresés")
  (icon-tooltip.filter "A keresés végrehajtása"))
(def resources en
  (icon-label.filter "Filter")
  (icon-tooltip.filter "Execute the filter"))

(def icon find)
(def resources hu
  (icon-label.find "Keresés")
  (icon-tooltip.find "Egy objektum keresése"))
(def resources en
  (icon-label.find "Find")
  (icon-tooltip.find "Find an object"))

(def icon set-to-nil)
(def resources hu
  (icon-label.set-to-nil "Szétkapcsolás")
  (icon-tooltip.set-to-nil "Az objektumok szétkapcsolása"))
(def resources en
  (icon-label.set-to-nil "Disconnect")
  (icon-tooltip.set-to-nil "Disconnect from object"))

(def icon set-to-unbound)
(def resources hu
  (icon-label.set-to-unbound "Alapértelmezett")
  (icon-tooltip.set-to-unbound "Az alapértelmezett értékre beállítása"))
(def resources en
  (icon-label.set-to-unbound "Default")
  (icon-tooltip.set-to-unbound "Set to default"))

(def icon select)
(def resources hu
  (icon-label.select "Kiválasztás")
  (icon-tooltip.select "Egy objektum kiválasztása"))
(def resources en
  (icon-label.select "Select")
  (icon-tooltip.select "Select an object"))

(def icon view)
(def resources hu
  (icon-label.view "Nézet")
  (icon-tooltip.view "Nézet váltás"))
(def resources en
  (icon-label.view "View")
  (icon-tooltip.view "Change view"))

(def icon finish)
(def resources hu
  (icon-label.finish "Befejezés")
  (icon-tooltip.finish "A varázsló befejezése"))
(def resources en
  (icon-label.finish "Finish")
  (icon-tooltip.finish "Finish wizard"))

(def icon cancel)
(def resources hu
  (icon-label.cancel "Mégse")
  (icon-tooltip.cancel "Az aktuális művelet félbeszakítása az esetleges változtatások elmentése nélkül"))
(def resources en
  (icon-label.cancel "Cancel")
  (icon-tooltip.cancel "Cancel the current operation without modifying anything"))
