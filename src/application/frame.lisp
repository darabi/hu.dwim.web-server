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
   (unique-counter 0)
   (frame-index (random-simple-base-string +frame-index-length+))
   (next-frame-index (random-simple-base-string +frame-index-length+))
   (client-state-sink-id->client-state-sink (make-hash-table :test 'equal))
   (action-id->action (make-hash-table :test 'equal))
   (root-component nil :export #t)
   (debug-component-hierarchy #f :type boolean)))

(def print-object (frame :identity #t :type #f)
  (print-object-for-string-id-mixin -self-)
  (write-string " index: ")
  (princ (frame-index-of -self-))
  (write-string " actions: ")
  (princ (hash-table-count (action-id->action-of -self-))))

(def function toggle-debug-component-hierarchy (frame)
  (setf (debug-component-hierarchy-p frame) (not (debug-component-hierarchy-p frame))))

(def (function ei) generate-frame-unique-string (&optional (prefix "_u") (frame *frame*))
  ;; TODO optimize
  (format nil "~A~A" prefix (incf (unique-counter-of frame))))

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
  (app.debug "Stepping to next frame index. From ~S to next ~S, frame is ~A" (frame-index-of frame) (next-frame-index-of frame) frame)
  (setf (frame-index-of frame) (next-frame-index-of frame))
  (setf (next-frame-index-of frame) (random-simple-base-string +frame-index-length+)))

(def (function e) reset-frame-root-component ()
  (setf (root-component-of *frame*) nil))


;;;;;;;;;;;;;;;;;;;;;;
;;; client state sinks

(def constant +client-state-sink-id-length+ 8)

(def class* client-state-sink (string-id-for-funcallable-mixin)
  ()
  (:metaclass funcallable-standard-class))

(def (macro e) make-client-state-sink ((value-variable-name) &body body)
  `(make-client-state-sink-using-lambda (lambda (,value-variable-name)
                                          ,@body)))

(def (function e) make-client-state-sink-using-lambda (client-state-sink-lambda)
  (bind ((client-state-sink (make-instance 'client-state-sink)))
    (set-funcallable-instance-function client-state-sink client-state-sink-lambda)
    client-state-sink))

(def (function e) register-client-state-sink (frame client-state-sink)
  (assert (or (not (boundp '*frame*))
              (null *frame*)
              (eq *frame* frame)))
  (assert-session-lock-held (session-of frame))
  (bind ((client-state-sink-id->client-state-sink (client-state-sink-id->client-state-sink-of frame)))
    (unless (typep client-state-sink 'client-state-sink)
      (assert (functionp client-state-sink))
      (setf client-state-sink (make-client-state-sink-using-lambda client-state-sink)))
    (bind ((client-state-sink-id (insert-with-new-random-hash-table-key
                                  client-state-sink-id->client-state-sink
                                  client-state-sink
                                  +client-state-sink-id-length+
                                  :prefix #.(coerce "_cs_" 'simple-base-string))))
      (setf (id-of client-state-sink) client-state-sink-id)
      (app.dribble "Registered client-state-sink with id ~S in frame ~A" client-state-sink-id frame)
      client-state-sink)))

(def function process-client-state-sinks (frame query-parameters)
  (bind ((client-state-sink-id->client-state-sink (client-state-sink-id->client-state-sink-of frame)))
    (iter (for (name . value) :in query-parameters)
          (app.dribble "Checking query parameter ~S if it's a client-state-sink" name)
          (awhen (gethash name client-state-sink-id->client-state-sink)
            (app.dribble "Query parameter ~S is a client-state-sink, calling it with value ~S" it value)
            (funcall it value)))))
