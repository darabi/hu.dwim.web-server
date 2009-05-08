;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Abstract menu item

(def component abstract-menu-item-component ()
  ((menu-items nil :type components)))

;;;;;;
;;; Menu

(def component menu-component (abstract-menu-item-component style-component-mixin)
  ((target-place nil :type place :export :accessor)
   (icon nil :type component)))

(def function flatten-menu-items (menu-items)
  (iter (for menu-item :in menu-items)
        (if (listp menu-item)
            (appending menu-item)
            (collect menu-item))))

(def (function e) make-menu-component (label menu-items)
  (bind ((menu-items (flatten-menu-items menu-items)))
    (when menu-items
      (make-instance 'menu-component
                     :icon (when label
                             (icon menu :label label :tooltip nil))
                     :menu-items menu-items))))

(def (macro e) menu (label &body menu-items)
  `(make-menu-component ,label (list ,@menu-items)))

(def function render-menu-items (menu-items)
  (mapcar #'render menu-items))

(def render-xhtml menu-component
  (bind (((:read-only-slots icon menu-items id css-class style) -self-))
    <div (:id ,id :class ,css-class :style ,style)
         ,(when icon (render icon))
         ,(foreach #'render menu-items)>))

(def icon menu) ;; TODO: icon

;;;;;;
;;; Menu item

(def component menu-item-component (abstract-menu-item-component style-component-mixin)
  ((command nil :type component)))

(def (function e) make-menu-item-component (command menu-items &key id css-class style)
  (bind ((menu-items (flatten-menu-items menu-items)))
    (when (or menu-items
              command)
      (make-instance 'menu-item-component
                     :command command
                     :menu-items menu-items
                     :id id
                     :css-class css-class
                     :style style))))

(def (macro e) menu-item ((&key id css-class style) command &body menu-items)
  `(make-menu-item-component ,command (list ,@menu-items) :id ,id :css-class ,css-class :style ,style))

(def render-xhtml menu-item-component
  (bind (((:read-only-slots command menu-items id css-class style) -self-))
    <div (:id ,id :class ,css-class :style ,style)
         ,(render command)
         ,(foreach #'render menu-items)>))

;;;;;;
;;; Replace menu target command

(def component replace-menu-target-command-component (command-component)
  ((component)))

(def constructor replace-menu-target-command-component ()
  (setf (action-of -self-)
        (make-action
          (bind ((menu-component
                  (find-ancestor-component -self-
                                           (lambda (ancestor)
                                             (and (typep ancestor 'menu-component)
                                                  (target-place-of ancestor)))))
                 (component (force (component-of -self-))))
            (setf (component-of -self-) component
                  (component-at-place (target-place-of menu-component)) component)))))

(def (macro e) replace-menu-target-command (content &body forms)
  `(make-instance 'replace-menu-target-command-component :content ,content :component (delay ,@forms)))

;;;;;;
;;; Debug menu

(def (function e) make-debug-menu ()
  (menu-item () "Debug"
    (menu-item ()
        (command "Start over"
                 (make-action (reset-frame-root-component))
                 :send-client-state #f))
    (menu-item ()
        (command "Toggle test mode"
                 (make-action (notf (running-in-test-mode-p *application*)))))
    (menu-item ()
        (command "Toggle profiling"
                 (make-action (notf (profile-request-processing-p *server*)))))
    (menu-item ()
        (command "Toggle hierarchy"
                 (make-action (toggle-debug-component-hierarchy *frame*))))
    (menu-item ()
        (command "Toggle debug client side"
                 (make-action (notf (debug-client-side? (root-component-of *frame*))))))
    ;; from http://turtle.dojotoolkit.org/~david/recss.html
    (menu-item ()
        (inline-component
          <a (:href "#"
              :class "command"
              :onClick `js-inline(wui.reload-css))
             "Reload CSS">))
    #+sbcl
    (menu-item ()
        (replace-menu-target-command "Frame size breakdown"
          (make-instance 'frame-size-breakdown-component)))
    (menu-item ()
        (replace-menu-target-command "Web server"
          (make-instance 'server-info)))))
