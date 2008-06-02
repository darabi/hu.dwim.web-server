;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* js-file-serving-broker (file-serving-broker)
  ()
  (:metaclass funcallable-standard-class))

#|
(defgeneric parenscript-handler-compile (handler)
  (:documentation "Called when (re)compilation is needed. Should return the compiled js string."))

(defgeneric parenscript-handler-timestamp (handler)
  (:documentation "Should return the timestamp of the source (used for dirtyness check)."))

(defmethod handler-handle ((handler parenscript-handler)
                           (application basic-application)
                           (context standard-request-context)
                           matcher-result)
  (with-accessors
      ((cachep cachep) (cached-javascript cached-javascript)
       (last-compiled last-compiled)) handler
    (ucw.rerl.dispatcher.debug "~S handling the request" handler)
    (when (or (not cachep)
              (not cached-javascript)
              (> (parenscript-handler-timestamp handler)
                 last-compiled))
      (ucw.rerl.dispatcher.debug "~S is compiling the file" handler)
      ;; enable a #"" syntax for i18n lookups in parenscript
      (let ((*readtable* (copy-readtable *readtable*)))
        (enable-js-sharpquote-syntax)
        (setf cached-javascript (string-to-octets (parenscript-handler-compile handler) :utf-8)))
      (setf last-compiled (get-universal-time)))
    (serve-sequence cached-javascript
                    :last-modified last-compiled
                    :content-type "text/javascript; charset=utf-8")))

|#
