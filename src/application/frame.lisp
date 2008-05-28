;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (constant :test 'string=) +frame-id-parameter-name+    "_f")
(def (constant :test 'string=) +frame-index-parameter-name+ "_x")

(def constant +frame-id-length+ 8)
(def constant +frame-index-length+ 4)

(def (special-variable e) *frame*)

(def generic purge-frames (application session))

(def (condition* e) frame-out-of-sync-error (request-processing-error)
  ((frame nil)))

(def (function i) frame-out-of-sync-error (&optional (frame *frame*))
  (error 'frame-out-of-sync-error :frame frame))

(def class* frame (string-id-mixin activity-monitor-mixin)
  ((session nil)
   (frame-index (random-simple-base-string +frame-index-length+))
   (next-frame-index (random-simple-base-string +frame-index-length+))
   (callback-id->callback (make-hash-table :test 'equal))
   (action-id->action (make-hash-table :test 'equal))
   (root-component nil)
   (debug-component-hierarchy #f :type boolean)))

(def print-object (frame :identity #t :type #f)
  (print-object-for-string-id-mixin self)
  (write-string " index: ")
  (princ (frame-index-of self))
  (write-string " actions: ")
  (princ (hash-table-count (action-id->action-of self))))

(def (function o) find-frame-from-request (session)
  (bind ((frame-id (parameter-value +frame-id-parameter-name+)))
    (when frame-id
      (app.debug "Found frame-id parameter ~S" frame-id)
      (bind ((frame (gethash frame-id (frame-id->frame-of session))))
        (if (and frame
                 (not (is-timed-out? frame)))
            (progn
              (app.debug "Looked up as valid frame ~A" frame)
              frame)
            (values))))))

(def method purge-frames (application (session session))
  (assert-session-lock-held session)
  (bind ((frame-id->frame (frame-id->frame-of session)))
    (iter (for (frame-id frame) :in-hashtable frame-id->frame)
          (when (is-timed-out? frame)
            (remhash frame-id frame-id->frame))))
  (values))

(def function step-to-next-frame-index (frame)
  (setf (frame-index-of frame) (next-frame-index-of frame))
  (setf (next-frame-index-of frame) (random-simple-base-string +frame-index-length+)))
