;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Remote identity mixin

(def component remote-identity-mixin ()
  ((id nil :type string :documentation "A life time frame unique identifier."))
  (:documentation "A component with a permanent frame unique remote identifier that is set at the first refresh and never changed afterwards."))

(def refresh remote-identity-mixin
  (bind (((:slots id) -self-))
    (unless id
      (setf id (generate-frame-unique-string "c")))))

(def (layered-function e) render-remote-setup (component)
  (:method :in xhtml-format ((component remote-identity-mixin))
    `js(on-load (wui.setup-component ,(id-of component) ,(instance-class-name-as-string component)))))

(def function collect-covering-remote-identity-components-for-dirty-descendant-components (component)
  ;; KLUDGE: find top-component and go down from there to avoid
  (setf component (find-descendant-component-with-type component 'top-mixin))
  (assert component nil "There is no TOP-MIXIN above ~A, AJAX cannot be used in this situation at the moment" component)
  (bind ((covering-components nil))
    (labels ((traverse (component)
               ;; NOTE: we cannot evaluate delayed visible here, because we are not in the component's environment
               (catch component
                 (with-component-environment component
                   (when (force (visible-p component))
                     (if (or (dirty-p component)
                             (outdated-p component))
                         (bind ((remote-identity-component (find-ancestor-component-with-type component 'remote-identity-mixin)))
                           (assert remote-identity-component nil "There is no ancestor component with remote identity for ~A" component)
                           (pushnew remote-identity-component covering-components)
                           (throw remote-identity-component nil))
                         (map-child-components component #'traverse)))))))
      (traverse component))
    (remove-if (lambda (component-to-be-removed)
                 (find-if (lambda (covering-component)
                            (and (not (eq component-to-be-removed covering-component))
                                 (find-ancestor-component component-to-be-removed [eq !1 covering-component])))
                          covering-components))
               covering-components)))

;;;;;;
;;; Remote setup mixin

(def component remote-setup-mixin (remote-identity-mixin)
  ()
  (:documentation "A component that will be unconditionally set up on the remote side using its remote identity."))

(def render-xhtml :after remote-setup-mixin
  (render-remote-setup -self-))
