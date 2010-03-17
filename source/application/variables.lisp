;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; specials available while processing a request

(def (special-variable e :documentation "The APPLICATION associated with the currently processed HTTP REQUEST.")
  *application*)

(def (special-variable e :documentation "The SESSION associated with the currently processed HTTP REQUEST.")
  *session*)

(def (special-variable e :documentation "The FRAME associated with the currently processed HTTP REQUEST.")
  *frame*)

(def (special-variable e :documentation "The ACTION associated with the currently processed HTTP REQUEST.")
  *action*)

(def (special-variable :documentation "This variable is bound in application contexts and set to T when the render protocol is invoked. Needed for the error handling code to decide what to do.")
  *rendering-phase-reached*)

(def (special-variable :documentation "This variable is bound in application contexts and set to T when the request processing reached the point of querying the entry points. Needed for the error handling code to decide what to do.")
  *inside-user-code*)

(def (special-variable e :documentation "Rebound when actions are processed and RENDER is called. When true, it means that it's a lazy request for some part of the screen whose rendering was delayed. AJAX requests are implicitly delayed content requests.")
  *delayed-content-request*)

(def (special-variable e :documentation "Rebound when actions are processed and RENDER is called. When true, it means that the request was fired by the client side JS stack and expects an XML answer.")
  *ajax-aware-request*)

(def (special-variable e :documentation "Bound inside entry-points, and contains part of the path of the request url that comes after the application's url prefix.")
  *application-relative-path*)

(def (special-variable e :documentation "Bound inside entry-points, and contains part of the path of the request url that comes after the entry-point url prefix (so, it's an empty string for exact path matching entry-point's).")
  *entry-point-relative-path*)

;;;;;;
;;; application slot defaults

;; TODO think through the 'default' prefix. somewhere added, somewhere not... maybe rebind from app context...

(def (special-variable e) *maximum-sessions-per-application-count* most-positive-fixnum
  "The default for the same slot in applications.")

(def (special-variable e) *default-session-timeout* (* 30 60)
  "The default for the same slot in applications.")

(def (special-variable e) *default-frame-timeout* *default-session-timeout*
  "The default for the same slot in applications.")

(def (special-variable e) *default-ajax-enabled* #t
  "The default for the same slot in applications.")

(def (special-variable e) *default-compile-time-debug-client-side* #f
  "The default for the same slot in applications.")

;;;;;;
;;; constants

(def constant +session-purge/time-interval+ 30)

(def constant +action-id-length+   8)
(def constant +frame-id-length+    8)
(def constant +frame-index-length+ 4)
(def constant +session-id-length+  40)

(def constant +action-id-parameter-name+       "_a")
(def constant +frame-id-parameter-name+        "_f")
(def constant +frame-index-parameter-name+     "_x")
(def constant +ajax-aware-parameter-name+      "_j")
(def constant +delayed-content-parameter-name+ "_d")
(def constant +timestamp-parameter-name+       "_ts")

;; TODO: merge these into a single parameter and provide accessor functions?
(def constant +shitf-key-parameter-name+       "_sk")
(def constant +control-key-parameter-name+     "_ck")
(def constant +alt-key-parameter-name+         "_ak")

(pushnew +ajax-aware-parameter-name+ *clone-request-uri/default-strip-query-parameters*)
(pushnew +delayed-content-parameter-name+ *clone-request-uri/default-strip-query-parameters*)

(def constant +frame-query-parameter-names+ (list +frame-id-parameter-name+
                                                  +frame-index-parameter-name+
                                                  +action-id-parameter-name+))

(def constant +session-cookie-name+ "sid")

;;;;;;
;;; Stuff needed for applications and components

(def (constant e) +scroll-x-parameter-name+ "_sx")
(def (constant e) +scroll-y-parameter-name+ "_sy")
(def (constant e) +no-javascript-error-parameter-name+ "_njs")

(def (constant e) +page-failed-to-load-id+ "page-failed-to-load")
(def (constant e) +page-failed-to-load-grace-period-in-millisecs+ 10000)
