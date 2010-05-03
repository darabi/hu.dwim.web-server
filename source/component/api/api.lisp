;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; TODO: move some not so generic functions from this file to the place their methods are first defined

;;;;;;
;;; Component factories for types

(def (layered-function e) make-maker (type &rest args &key value &allow-other-keys)
  (:documentation "Creates a COMPONENT that creates new values of TYPE, initialized from VALUE. Other ARGS are passed to the COMPONENT being created."))

(def (layered-function e) make-viewer (type &rest args &key value &allow-other-keys)
  (:documentation "Creates a COMPONENT that displays existing values of TYPE, initialized to VALUE. Other ARGS are passed to the COMPONENT being created. Call COMPNENT-VALUE-OF to get, and (SETF COMPONENT-VALUE-OF) to set the VALUE."))

(def (layered-function e) make-editor (type &rest args &key value &allow-other-keys)
  (:documentation "Creates a COMPONENT that edits existing values of TYPE, initialized to VALUE. Other ARGS are passed to the COMPONENT being created. Call COMPNENT-VALUE-OF to get, and (SETF COMPONENT-VALUE-OF) to set the VALUE."))

(def (layered-function e) make-inspector (type &rest args &key value &allow-other-keys)
  (:documentation "Creates a COMPONENT that displays or edits existing values of TYPE, initialized to VALUE. Other ARGS are passed to the COMPONENT being created. Call COMPNENT-VALUE-OF to get, and (SETF COMPONENT-VALUE-OF) to set the VALUE. The inspector is essentially a viewer and an editor at the same time, and the user can switch between the two modes."))

(def (layered-function e) make-filter (type &rest args &key value &allow-other-keys)
  (:documentation "Creates a COMPONENT that filters the set of existing values of TYPE based on a filter criteria provided by the user, initialized from VALUE. Other ARGS are passed to the COMPONENT being created. Call COMPNENT-VALUE-OF to get, and (SETF COMPONENT-VALUE-OF) to set the TYPE."))

(def (layered-function e) make-finder (type &rest args &key value &allow-other-keys)
  (:documentation "Creates a COMPONENT that searches for a particular existing value of TYPE based on a filter criteria provided by the user, initialized from VALUE. Other ARGS are passed to the COMPONENT being created. Call COMPNENT-VALUE-OF to get, and (SETF COMPONENT-VALUE-OF) to set the TYPE."))

(def (layered-function e) make-selector (type &rest args &key value &allow-other-keys)
  (:documentation "Creates a COMPONENT that displays all existing values of TYPE to select exactly one value of them, initialized from VALUE. Other ARGS are passed to the COMPONENT being created."))

;;;;;;
;;; Component factories for values

(def (function e) make-value-viewer (value &rest args)
  "Creates a COMPONENT that displays VALUE and other similar values of the VALUE's type. Other ARGS are passed to the COMPONENT being created."
  (apply #'make-viewer (type-of value) :value value args))

(def (function e) make-value-editor (value &rest args)
  "Creates a COMPONENT that edits VALUE and other similar values of the VALUE's type. Other ARGS are passed to the COMPONENT being created."
  (apply #'make-editor (type-of value) :value value args))

(def (function e) make-value-inspector (value &rest args)
  "Creates a COMPONENT that displays or edits VALUE and other similar values of the VALUE's TYPE. Other ARGS are passed to the COMPONENT being created. The inspector is essentially a viewer and an editor at the same time, and the user can switch between the two modes."
  (apply #'make-inspector (type-of value) :value value args))

;;;;;;
;;; Component factories

(def (layered-function e) make-title (component class prototype value)
  (:documentation "Creates a TITLE for COMPONENT."))

(def (layered-function e) make-title-bar (component class prototype value)
  (:documentation "Creates a TITLE-BAR for COMPONENT."))

(def (layered-function e) make-header (component class prototype value)
  (:documentation "Creates a HEADER for COMPONENT."))

(def (layered-function e) make-footer (component class prototype value)
  (:documentation "Creates a FOOTER for COMPONENT."))

(def (layered-function e) make-command-bar (component class prototype value)
  (:documentation "Creates a COMMAND-BAR for COMPONENT."))

(def (layered-function e) make-command-bar-commands (component class prototype value)
  (:documentation "Creates a list of COMMANDs for the COMPONENT's COMMAND-BAR."))

(def (layered-function e) make-tool-bar (component class prototype value)
  (:documentation "Creates a TOOL-BAR for COMPONENT."))

(def (layered-function e) make-tool-bar-items (component class prototype value)
  (:documentation "Creates a list of TOOL-ITEMs for the COMPONENT's TOOL-BAR."))

(def (layered-function e) make-menu-bar (component class prototype value)
  (:documentation "Creates a MENU-BAR for COMPONENT."))

(def (layered-function e) make-menu-bar-items (component class prototype value)
  (:documentation "Creates a tree of MENU-ITEMs or COMMANDs for the menu hierarchy of the COMPONENT's MENU-BAR."))

(def (layered-function e) make-context-menu (component class prototype value)
  (:documentation "Creates a CONTEXT-MENU for COMPONENT."))

(def (layered-function e) make-context-menu-items (component class prototype value)
  (:documentation "Creates a tree of MENU-ITEMs or COMMANDs for the menu hierarchy of the COMPONENT's CONTEXT-MENU."))

(def (layered-function e) make-alternatives (component class prototype value)
  (:documentation "Creates a list of alternative views for COMPONENT, each being another COMPONENT."))

(def (layered-function e) make-reference-content (component class prototype value)
  (:documentation "Creates a COMPONENT to be the content of a reference to VALUE."))

(def (layered-function e) make-context-sensitive-help (component class prototype value)
  (:documentation "Creates a COMPONENT that provides help using the given COMPONENT."))

(def (layered-function e) make-content-presentation (component class prototype value)
  (:documentation "TODO"))

;;;;;;
;;; Command factories

(def (layered-function e) make-refresh-component-command (component class prototype value)
  (:documentation "Creates a COMMAND that calls REFRESH-COMPONENT."))

(def (layered-function e) make-close-component-command (component class prototype value)
  (:documentation "Create a COMMAND that permanently closes COMPONENT."))

(def (layered-function e) make-open-in-new-frame-command (component class prototype value)
  (:documentation "Creates a COMMAND that opens COMPONENT in a new FRAME."))

(def (layered-function e) make-expand-reference-command (reference class target expansion)
  (:documentation "TODO"))

(def (layered-function e) make-hide-component-command (component class prototype value)
  (:documentation "Creates a COMMAND that calls HIDE-COMPONENT."))

(def (layered-function e) make-show-component-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-show-component-recursively-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-toggle-visiblity-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-switch-to-alternative-commands (component class prototype value)
  (:documentation "Creates a list of COMMANDs to switch between alternative views for COMPONENT with SWITCH-TO-ALTERNATIVE."))

(def (layered-function e) make-switch-to-alternative-command (component class prototype value alternative)
  (:documentation "TODO"))

(def (layered-function e) make-switch-to-tab-page-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-go-to-first-page-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-go-to-previous-page-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-go-to-next-page-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-go-to-last-page-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-begin-editing-command (component class prototype value)
  (:documentation "Creates a COMMAND that starts EDITING under COMPONENT by calling BEGIN-EDITING."))

(def (layered-function e) make-save-editing-command (component class prototype value)
  (:documentation "Creates a COMMAND that actually saves the changes under COMPONENT by calling SAVE-EDITING and leaves EDITING by calling LEAVE-EDITING."))

(def (layered-function e) make-cancel-editing-command (component class prototype value)
  (:documentation "Creates a COMMAND that reverts the changes present under an COMPONENT and leaves EDITING."))

(def (layered-function e) make-store-editing-command (component class prototype value)
  (:documentation "Creates a COMMAND that actually stores the changes present under COMPONENT."))

(def (layered-function e) make-revert-editing-command (component class prototype instance)
  (:documentation "Creates a COMMAND reverts the changes present under COMPONENT."))

(def (layered-function e) make-editing-commands (component class prototype instance)
  (:documentation "Creates a list of COMMANDs for BEGIN-EDITING, SAVE-EDITING, CANCEL-EDITING, STORE-EDITING and REVERT-EDITING."))

(def (layered-function e) make-expand-component-command (component class prototype value)
  (:documentation "Creates a COMMAND to expand the COMPONENT to another COMPONENT with more detail."))

(def (layered-function e) make-collapse-component-command (component class prototype value)
  (:documentation "Creates a COMMAND to collapse the COMPONENT to another COMPONENT with less detail."))

(def (layered-function e) make-context-sensitive-help-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-export-command (format component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-export-commands (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-focus-command (component classs prototype value)
  (:documentation "Creates a COMMAND that replaces the TOP level COMPONENT usually found under the FRAME with the given COMPONENT."))

(def (layered-function e) make-move-commands (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-select-instance-command (component class prototype instance)
  (:documentation "TODO"))

(def (layered-function e) make-execute-filter-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-make-new-instance-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-select-component-command (component class prototype value)
  (:documentation "TODO"))

(def (layered-function e) make-begin-editing-new-instance-command (component class prototype)
  (:documentation "TODO"))

(def (layered-function e) make-create-instance-command (component class prototype)
  (:documentation "TODO"))

(def (layered-function e) make-evaluate-form-command (component class prototype value)
  (:documentation "TODO"))

;;;;;;
;;; Clone component

(def (generic e) clone-component (component)
  (:documentation "Returns a new CLONE of COMPONENT having the state of COMPONENT copied over."))

;;;;;;
;;; Close component

(def (generic e) close-component (component class prototype value)
  (:documentation "TODO"))

;;;;;;
;;; Open component in new frame

(def (generic e) open-in-new-frame (component class prototype value)
  (:documentation "Opens a new FRAME with the result of CLONE-COMPONENT called on COMPONENT."))

;;;;;;
;;; Alternative component views

(def (generic e) switch-to-alternative (component alternative)
  (:documentation "TODO"))

;;;;;;
;;; Show/hide component

(def (generic e) hideable-component? (component)
  (:documentation "TRUE means COMPONENT can be VISIBLE/HIDDEN, FALSE otherwise."))

(def (generic e) visible-component? (component)
  (:documentation "TRUE means COMPONENT is currently VISIBLE, FALSE means HIDDEN. If a COMPONENT is HIDDEN then it is not rendered at all."))

(def (generic e) hide-component (component)
  (:documentation "Hides a VISIBLE COMPONENT."))

(def (generic e) show-component (component)
  (:documentation "Shows a HIDDEN COMPONENT."))

(def (generic e) hide-component-recursively (component)
  (:documentation "Hides a VISIBLE COMPONENT and all its VISIBLE descendants."))

(def (generic e) show-component-recursively (component)
  (:documentation "Shows a HIDDEN COMPONENT and all its HIDDEN descendants."))

;;;;;;
;;; Enable/disable component

(def (generic e) disableable-component? (component)
  (:documentation "TRUE means COMPONENT can be DISABLED/ENABLED, FALSE otherwise."))

(def (generic e) enabled-component? (component)
  (:documentation "TRUE means COMPONENT is currently ENABLED, FALSE means DISABLED."))

(def (generic e) disable-component (component)
  (:documentation "Puts COMPONENT into DISABLED state."))

(def (generic e) enable-component (component)
  (:documentation "Puts COMPONENT into ENABLED state."))

(def (generic e) disable-component-recursively (component)
  (:documentation "Puts COMPONENT and all its ENABLED descendants into DISABLED state."))

(def (generic e) enable-component-recursively (component)
  (:documentation "Puts COMPONENT and all its DISABLED descendants into ENABLED state."))

;;;;;;
;;; Expand/collapse component

(def (generic e) collapsible-component? (component)
  (:documentation "TRUE means COMPONENT can be EXPANDED/COLLAPSED, FALSE otherwise."))

(def (generic e) expanded-component? (component)
  (:documentation "TRUE means COMPONENT is currently EXPANDED, FALSE means COLLAPSED."))

(def (generic e) collapse-component (component)
  (:documentation "Collapses an EXPANDED COMPONENT."))

(def (generic e) expand-component (component)
  (:documentation "Expands a COLLAPSED COMPONENT."))

(def (generic e) collapse-component-recursively (component)
  (:documentation "Collapes an EXPANDED COMPONENT and all its EXPANDED descendants."))

(def (generic e) expand-component-recursively (component)
  (:documentation "Expands a COLLAPSED COMPONENT and all its COLLAPSED descendants."))

;;;;;;
;;; Export component

(def (layered-function e) export-text (component)
  (:documentation "Export COMPONENT into pure Text."))

(def (layered-function e) export-csv (component)
  (:documentation "Export COMPONENT into Comma Separated Values."))

(def (layered-function e) export-pdf (component)
  (:documentation "Export COMPONENT into Portable Document Format."))

(def (layered-function e) export-odt (component)
  (:documentation "Export COMPONENT into Open Office Text Document."))

(def (layered-function e) export-ods (component)
  (:documentation "Export COMPONENT into Open Office Spreadsheet Document."))

(def (layered-function e) export-sh (component)
  (:documentation "Export COMPONENT into Shell Script."))

;;;;;;
;;; Serialize/deserialize component

(def (generic e) serialize-component (component)
  (:documentation "Writes a COMPONENT into a byte vector to be read later by DESERIALIZE-COMPONENT."))

(def (generic e) deserialize-component (bytes)
  (:documentation "Reads a COMPONENT from a byte vector previously written by SERIALIZE-COMPONENT."))

;;;;;;
;;; Component message

(def (generic e) add-component-information-message (component message &rest message-args)
  (:documentation "Adds an information message to COMPONENT similar to a FORMAT string with arguments."))

(def (generic e) add-component-warning-message (component message &rest message-args)
  (:documentation "Adds a warning message to COMPONENT similar to a FORMAT string with arguments."))

(def (generic e) add-component-error-message (component message &rest message-args)
  (:documentation "Adds an error message to COMPONENT similar to a FORMAT string with arguments."))

(def (generic e) add-component-message (component message message-args &key category &allow-other-keys)
  (:documentation "Adds a generic message to COMPONENT similar to a FORMAT string with arguments."))

;;;;;;
;;; Component dispatch class/prototype

(def (generic e) component-dispatch-class (component)
  (:documentation "Returns a STANDARD-CLASS instance for COMPONENT, factory methods use it as a dispatch argument to support customization."))

(def (generic e) component-dispatch-prototype (component)
  (:documentation "Returns a STANDARD-OBJECT instance for COMPONENT, factory methods use it as a dispatch argument to support customization."))

;;;;;;
;;; Component documentation

(def (generic e) component-documentation (component)
  (:documentation "Returns a COMPONENT that documents the received COMPONENT, often it is just a STRING."))

;;;;;;
;;; Component style class

(def (generic e) component-style-class (component)
  (:documentation "Returns a string designating the appropriate style for the COMPONENT."))

;;;;;;
;;; Component value

(def (generic e) component-value-of (component)
  (:documentation "Returns the COMPONENT-VALUE associated with COMPONENT."))

(def (generic e) (setf component-value-of) (new-value component)
  (:documentation "Sets the COMPONENT-VALUE associated with COMPONENT to NEW-VALUE."))

(def (generic e) reuse-component-value (component class prototype value)
  (:documentation "Carries out anything needed to reuse the COMPONENT-VALUE of COMPONENT and returns it."))

;;;;;;
;;; Component editing

(def (generic e) editable-component? (component)
  (:documentation "TRUE means COMPONENT can be switched between edited and non edited mode, FALSE otherwise."))

(def (generic e) edited-component? (component)
  (:documentation "TRUE menas COMPONENT is currently being edited, FALSE otherwise."))

(def (generic e) begin-editing (component)
  (:documentation "Starts EDITING under COMPONENT."))

(def (generic e) save-editing (component)
  (:documentation "Saves changes under COMPONENT and leaves EDITING."))

(def (generic e) cancel-editing (component)
  (:documentation "Reverts changes under COMPONENT and leaves EDITING."))

(def (generic e) store-editing (component)
  (:documentation "Stores changes under COMPONENT without leaving EDITING.."))

(def (generic e) revert-editing (component)
  (:documentation "Reverts changes under COMPONENT without leaving EDITING.."))

(def (generic e) join-editing (component)
  (:documentation "Modifies COMPONENT to join into EDITING."))

(def (generic e) leave-editing (component)
  (:documentation "Modifies COMPONENT to leave EDITING."))

;;;;;;
;;; Component environment

(def (generic e) call-in-rendering-environment (application session thunk)
  (:documentation "Call THUNK within the rendering environment of APPLICATION and SESSION."))

(def (generic e) call-in-component-environment (component thunk)
  (:documentation "Sets up dynamic COMPONENT-ENVIRONMENT and calls THUNK."))

(def (definer e) component-environment (&body forms)
  "Defines the COMPONENT-ENVIRONMENT for a specific COMPONENT class."
  (bind ((qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms))
         (unused (gensym)))
    `(def method call-in-component-environment ,@(when qualifier (list qualifier)) ((-self- ,type) ,unused)
       ,@forms)))

;;;;;;
;;; Parent component

(def (generic e) parent-component-of (component)
  (:documentation "Returns the parent of COMPONENT in the hierarchy or NIL if it is the root COMPONENT."))

(def (generic e) (setf parent-component-of) (new-value component)
  (:documentation "Sets the parent of COMPONENT to NEW-VALUE in the hierarchy. This is called automatically when setting a COMPONENT into a COMPONENT-SLOT-DEFINITION."))

(def (generic e) child-component-slot? (component slot)
  (:documentation "TRUE if SLOT is considered to have child COMPONENTs in it, FALSE otherwise."))

;;;;;;
;;; Component place

(def (generic e) component-at-place (place)
  (:documentation "Returns the current COMPONENT or NIL at PLACE."))

(def (generic e) (setf component-at-place) (component place)
  (:documentation "Sets COMPONENT into PLACE and takes care about keeping the COMPONENT hierachy uptodate with respect to PARENT-COMPONENT-OF."))

(def (generic e) make-component-place (component)
  (:documentation "Returns a PLACE or NIL if no such PLACE can be found for COMPONENT. The COMPONENT must be able to provide a parent COMPONENT to find its PLACE."))

;;;;;;
;;; Render component

(def (layered-function e) render-component (component)
  (:documentation "Renders COMPONENT according to the current RENDER-COMPONENT-LAYER either by returning the result or by doing side effects (to a stream for example)."))

(def (layered-function e) render-component-stub (component)
  (:documentation "Renders COMPONENT as a stub that can be later replaced by an incremental render."))

(def (generic e) lazily-rendered-component? (component)
  (:documentation "TRUE means COMPONENT does not need to be fully rendered to the remote side, FALSE otherwise."))

(def (generic e) to-be-rendered-component? (component)
  (:documentation "TRUE means COMPONENT needs to be rendered to the remote side, FALSE otherwise."))

(def (generic e) mark-to-be-rendered-component (component)
  (:documentation "Marks COMPONENT to be rendered next time RENDER-COMPONENT is called."))

(def (generic e) mark-rendered-component (component)
  (:documentation "Marks COMPONENT as having been rendered by RENDER-COMPONENT."))

(def (definer e :available-flags "do") render-component (&whole whole &body forms)
  "Defines a generic RENDER-COMPONENT method for a specific COMPONENT class."
  (single-argument-layered-method-definer 'render-component forms :options -options- :whole whole))

;;;;;;
;;; Render component layer

(def (definer e :available-flags "e") render-component-layer (name supers documentation)
  "Defines a RENDER-COMPONENT-LAYER so that RENDER-COMPONENT and various other protocols can be customized on it."
  (bind ((layer-name (format-symbol *package* "~A-LAYER" name))
         (render-definer-name (format-symbol *package* "RENDER-~A" name))
         (super-layer-names (mapcar [format-symbol *package* "~A-LAYER" !1] supers)))
    `(progn
       (def (layer ,@-options-) ,layer-name ,super-layer-names ())
       (def (function ,@-options-) ,render-definer-name (component)
         (with-active-layers (,layer-name)
           (render-component component)))
       (def (definer ,@-options- :available-flags "do") ,render-definer-name (&whole whole &body forms)
         ,documentation
         (single-argument-layered-method-definer 'render-component forms :default-layer ',layer-name :options -options- :whole whole)))))

(def (special-variable e :documentation "The output stream for rendering components in text format.") *text-stream*)

(def (layer e) backend-layer ()
  ()
  (:documentation "Base layer for various rendering backends."))

(def (layer e) raw-name-layer ()
  ()
  (:documentation "Rendering without localizing names."))

(def (layer e) offline-layer ()
  ()
  (:documentation "Rendering into offline content that works without the server."))

(def (layer e) passive-layer ()
  ()
  (:documentation "Rendering into passive content that does not provide behaviour."))

(def (render-component-layer e) text (backend) "Rendering into pure Text.")

(def (render-component-layer e) sh (text) "Rendering into Shell Script.")

(def (render-component-layer e) xhtml (backend) "Rendering into XHTML with JavaScript.")

(def (render-component-layer e) offline-xhtml (offline xhtml) "Rendering into offline XHTML with JavaScript that works without the server.")

(def (render-component-layer e) passive-xhtml (passive xhtml) "Rendering into passive XHTML with JavaScript that does not provide behaviour.")

(def (render-component-layer e) csv (backend) "Rendering into Comma Separated Values.")

(def (render-component-layer e) pdf (backend) "Rendering into Portable Document Format.")

(def (render-component-layer e) ods (backend) "Rendering into Open Office Spreadsheet Document.")

(def (render-component-layer e) odt (backend) "Rendering into Open Office Text Document.")

;;;;;;
;;; Refresh component

(def (layered-function e) refresh-component (component)
  (:documentation "Refreshes child COMPONENTs under COMPONENT based on some state related to it (usually its COMPONENT-VALUE)."))

(def (generic e) to-be-refreshed-component? (component)
  (:documentation "TRUE means COMPONENT needs to be refreshed before the next RENDER-COMPONENT call, FALSE otherwise."))

(def (generic e) mark-to-be-refreshed-component (component)
  (:documentation "Marks COMPONENT to be refreshed by calling REFRESH-COMPONENT before the next RENDER-COMPONENT."))

(def (generic e) mark-refreshed-component (component)
  (:documentation "Marks COMPONENT as having been refreshed after a successful call to REFRESH-COMPONENT."))

(def (definer e :available-flags "do") refresh-component (&whole whole &body forms)
  "Defines a generic REFRESH-COMPONENT for a specific COMPONENT class."
  (single-argument-layered-method-definer 'refresh-component (cons :after forms) :options -options- :whole whole))

;;;;;;
;;; Debug component hierarchy

(def (special-variable e) *debug-component-hierarchy* #f
  "Specifies whether COMPONENTs are rendered with debug information or not. TRUE means each component will have some special COMMANDs to inspect, copy to the REPL, etc.")

(def (generic e) supports-debug-component-hierarchy? (component)
  (:documentation "TRUE means that COMPONENT supports rendering with debug information, FALSE otherwise. This is used by some rendering backends, namely XHTML does not support wrapping TR and TD elements with arbitrary DIV elements within a TABLE."))

;;;;;;
;;; Print component

(def (special-variable e) *component-print-level* 0
  "Specifies the current print level in terms of COMPONENT hierarchy depth during printing a COMPONENT, printing COMPONENTs deeper than *PRINT-LEVEL* is skipped.")

(def (generic e) print-component (component &optional stream)
  (:documentation "Prints a string representation of COMPONENT into STREAM, this is the default behaviour for the PRINT-OBJECT method of COMPONENT."))
