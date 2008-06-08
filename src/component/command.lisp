;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (layer e) passive-components-layer ()
  ())

;;;;;;
;;; Command

(def component command-component ()
  ((visible #t)
   (enabled #t)
   (tooltip-emitter nil)
   (icon nil :type component)
   (action)))

(def (macro e) command (icon action &rest args &key &allow-other-keys)
  `(make-instance 'command-component :icon ,icon :action ,action ,@args))

(def render command-component ()
  (with-slots (visible enabled icon action tooltip-emitter) -self-
    (when (force visible)
      (if (force enabled)
          (bind ((href (etypecase action
                         (action (action-to-href action))
                         ;; TODO wastes of resources. store back the printed uri? see below also...
                         (uri (print-uri-to-string action))
                         (string action)))
                 (id (when tooltip-emitter
                       (generate-frame-unique-string))))
            ;; render the `js first, so the return value contract of qq is kept.
            (when tooltip-emitter
              ;; TODO this could be collected and emitted using a (map ... data) to spare some space
              `js(on-load
                  (new dojox.widget.DynamicTooltip
                       (create :connectId (array ,id)
                               :position (array "below" "right")
                               :href ,(etypecase tooltip-emitter
                                        (action (action-to-href tooltip-emitter :delayed-content #t))
                                        (uri (print-uri-to-string tooltip-emitter))
                                        (string tooltip-emitter))))))
            <a (:id ,id
                :href "#"
                :title ,(when (and (not tooltip-emitter)
                                   (typep icon 'icon-component))
                          (force (tooltip-of icon)))
                :onclick `js-inline(submit-form ,href))
               ,(if (typep icon 'icon-component)
                    (render-icon (image-path-of icon) :label (label-of icon))
                    (render icon))>)
          (render icon)))))

(def render :in passive-components-layer command-component
  (with-slots (visible icon) -self-
    (when (force visible)
      (render icon))))

;;;;;;
;;; Command bar

(def component command-bar-component ()
  ((commands :type components)))

(def (macro e) command-bar (&body commands)
  `(make-instance 'command-bar-component :commands (list ,@commands)))

(def render command-bar-component ()
  (with-slots (commands parent-component) -self-
    (setf commands (sort-commands parent-component commands))
    (render-horizontal-list commands :css-class "command-bar")))

(def render :in passive-components-layer command-bar-component
  (values))

(def generic find-command-bar (component)
  (:method ((component component))
    (map-child-components component
                          (lambda (child)
                            (when (typep child 'command-bar-component)
                              (return-from find-command-bar child))))))

(def generic sort-commands (component commands)
  (:method ((component component) commands)
    (sort commands #'<
          :key (lambda (command)
                 (or (position (name-of (icon-of command))
                               '(answer back open-in-new-frame top collapse collapse-all expand-all refresh new edit save cancel store revert delete)
                               :test #'equal)
                     most-positive-fixnum)))))

(def function push-command (command component)
  "Push a new COMMAND into the COMMAND-BAR of COMPONENT"
  (bind ((command-bar (find-command-bar component)))
    (assert command-bar nil "No command bar found, no place to push ~A in ~A" command component)
    (setf (commands-of command-bar)
          (sort-commands component (cons command (commands-of command-bar))))))

(def function pop-command (command)
  "Pop the COMMAND from the container COMMAND-BAR"
  (removef (commands-of (parent-component-of command)) command))

;;;;;;
;;; Navigation bar

(def component page-navigation-bar-component (command-bar-component)
  ((position)
   (page-count)
   (total-count)
   (first-command :type component)
   (previous-command :type component)
   (next-command :type component)
   (last-command :type component)
   (jumper :type component)))

(def constructor page-navigation-bar-component ()
  (with-slots (position page-count total-count first-command previous-command next-command last-command jumper) -self-
    (setf first-command (make-instance 'command-component
                                       :icon (icon first)
                                       :enabled (delay (> position 0))
                                       :action (make-action (setf position 0)))
          previous-command (make-instance 'command-component
                                          :icon (icon previous)
                                          :enabled (delay (> position 0))
                                          :action (make-action (decf position (min position page-count))))
          next-command (make-instance 'command-component
                                      :icon (icon next)
                                      :enabled (delay (< position (- total-count page-count)))
                                      :action (make-action (incf position (min page-count (- total-count page-count)))))
          last-command (make-instance 'command-component
                                      :icon (icon last)
                                      :enabled (delay (< position (- total-count page-count)))
                                      :action (make-action (setf position (- total-count page-count))))
          jumper (make-instance 'integer-component :edited #t :component-value position))))

(def render page-navigation-bar-component ()
  (with-slots (first-command previous-command next-command last-command jumper) -self-
    (render-horizontal-list (list first-command previous-command jumper next-command last-command))))

;;;;;;
;;; Generic commands

(def generic refresh-component (component)
  (:method ((component component))
    (map-child-components component #'refresh-component)))

(def (function e) make-refresh-command (component)
  (make-instance 'command-component
                 :icon (icon refresh)
                 :action (make-action (refresh-component component))))

(def (function e) make-replace-command (original-component replacement-component &rest replace-command-args)
  "The REPLACE command replaces ORIGINAL-COMPONENT with REPLACEMENT-COMPONENT"
  (apply #'make-instance 'command-component
         :action (make-action
                   (bind ((replacement-component (force replacement-component))
                          (original-component (force original-component))
                          (original-place (make-component-place original-component)))
                     (setf (component-at-place original-place) replacement-component)))
         replace-command-args))

(def (function e) make-back-command (original-component original-place replacement-component replacement-place &rest args)
  "The BACK command puts REPLACEMENT-COMPONENT and ORIGINAL-COMPONENT back to their COMPONENT-PLACE and removes itself from its COMMAND-BAR"
  (prog1-bind command (apply #'make-instance 'command-component args)
    (setf (action-of command) (make-action
                                (pop-command command)
                                (when original-place
                                  (setf (component-at-place original-place) (force original-component)))
                                (when replacement-place
                                  (setf (component-at-place replacement-place) (force replacement-component)))))))

(def (function e) make-replace-and-push-back-command (original-component replacement-component replace-command-args back-command-args)
  "The REPLACE command replaces ORIGINAL-COMPONENT with REPLACEMENT-COMPONENT and pushes a BACK command into the COMMAND-BAR of REPLACEMENT-COMPONENT to revert the changes"
  (apply #'make-instance 'command-component
         :action (make-action
                   (bind ((replacement-component (force replacement-component))
                          (replacement-place (make-component-place replacement-component))
                          (original-component (force original-component))
                          (original-place (make-component-place original-component))
                          (back-command (apply #'make-back-command original-component original-place replacement-component replacement-place
                                               (append back-command-args
                                                       `(:visible ,(delay (and (not (has-edited-descendant-component-p replacement-component))
                                                                               (eq (force replacement-component) (component-at-place original-place)))))))))
                     (push-command back-command replacement-component)
                     (setf (component-at-place original-place) replacement-component)))
         replace-command-args))

(def function find-top-component-content (component)
  (awhen (find-ancestor-component-with-type component 'top-component)
    (content-of it)))

(def (function e) make-top-command (replacement-component)
  "The TOP command replaces the top level COMPONENT usually found under the FRAME with the given REPLACEMENT-COMPONENT"
  (bind ((original-component (delay (find-top-component-content replacement-component))))
    (make-replace-and-push-back-command original-component  replacement-component
                                        (list :icon (icon top)
                                              :visible (delay (not (eq replacement-component (find-top-component-content replacement-component)))))
                                        (list :icon (icon back)))))

(def (function e) make-open-in-new-frame-command (component)
  (make-instance 'command-component
                 :icon (icon open-in-new-frame)
                 ;; TODO: open a new frame with the component
                 :action (make-action #+nil (eseho:make-eseho-frame-component-with-content (clone-component component)))))
