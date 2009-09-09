;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Specials available while processing a request

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

(def (special-variable e :documentation "Rebound when actions are processed and RENDER is called. When true, it means that the request was fired by the remote JS stack and awaits a structured XML answer.")
  *ajax-aware-request*)

(def (special-variable e :documentation "Bound inside entry-points, and contains part of the path of the request url that comes after the application's url prefix.")
  *application-relative-path*)

(def (special-variable e :documentation "Bound inside entry-points, and contains part of the path of the request url that comes after the entry-point url prefix (so, it's an empty string for exact path matching entry-point's).")
  *entry-point-relative-path*)

(def constant +action-id-length+   8)
(def constant +frame-id-length+    8)
(def constant +frame-index-length+ 4)
(def constant +session-id-length+  40)

(def (constant :test 'string=) +action-id-parameter-name+       "_a")
(def (constant :test 'string=) +frame-id-parameter-name+        "_f")
(def (constant :test 'string=) +frame-index-parameter-name+     "_x")
(def (constant :test 'string=) +ajax-aware-parameter-name+      "_j")
(def (constant :test 'string=) +delayed-content-parameter-name+ "_d")

(pushnew +ajax-aware-parameter-name+ *clone-request-uri/default-strip-query-parameters*)
(pushnew +delayed-content-parameter-name+ *clone-request-uri/default-strip-query-parameters*)

(def (constant :test 'equal) +frame-query-parameter-names+ (list +frame-id-parameter-name+
                                                                 +frame-index-parameter-name+
                                                                 +action-id-parameter-name+))

(def (constant :test 'string=) +session-cookie-name+ "sid")

(def constant +epoch-start+ (encode-universal-time 0 0 0 1 1 1970 0))

;;;;;;
;;; Stuff needed for applications and components

(def (constant e :test 'string=) +scroll-x-parameter-name+ "_sx")
(def (constant e :test 'string=) +scroll-y-parameter-name+ "_sy")
(def (constant e :test 'string=) +no-javascript-error-parameter-name+ "_njs")

(def (constant e :test 'string=) +page-failed-to-load-id+ "_failed-to-load")
(def (constant e) +page-failed-to-load-grace-period-in-millisecs+ 10000)

(def (special-variable e) *dojo-skin-name* "tundra")
(def (special-variable e) *dojo-file-name* "dojo.js")
(def (special-variable e) *dojo-directory-name* "dojo/")

(def (constant e :test (constantly #t)) +mozilla-version-scanner+ (cl-ppcre:create-scanner "Mozilla/([0-9]{1,}\.[0-9]{0,})"))
(def (constant e :test (constantly #t)) +opera-version-scanner+ (cl-ppcre:create-scanner "Opera/([0-9]{1,}\.[0-9]{0,})"))
(def (constant e :test (constantly #t)) +msie-version-scanner+ (cl-ppcre:create-scanner "MSIE ([0-9]{1,}\.[0-9]{0,})"))
(def (constant e :test (constantly #t)) +drakma-version-scanner+ (cl-ppcre:create-scanner "Drakma/([0-9]{1,}\.[0-9]{0,})"))
