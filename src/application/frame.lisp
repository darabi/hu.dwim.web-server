;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def generic purge-frames (application session))

(def (condition* e) frame-related-request-processing-error (request-processing-error)
  ((frame nil)))

(def (condition* e) frame-index-missing-error (frame-related-request-processing-error)
  ())
(def function frame-index-missing-error (&optional (frame *frame*))
  (error 'frame-index-missing-error :frame frame))

(def (condition* e) frame-not-found-error (frame-related-request-processing-error)
  ())
(def function frame-not-found-error (&optional (frame *frame*))
  (error 'frame-not-found-error :frame frame))

(def function generate-frame-index (&optional previous)
  (if (and (running-in-test-mode? *application*)
           (or (not previous)
               (integerp previous)))
      (if previous
          (1+ previous)
          0)
      (random-simple-base-string +frame-index-length+)))

(def class* frame (string-id-mixin activity-monitor-mixin)
  ((session nil)
   (unique-counter 0)
   (frame-index (generate-frame-index))
   (next-frame-index (generate-frame-index 0))
   (client-state-sink-id->client-state-sink (make-hash-table :test 'equal))
   (action-id->action (make-hash-table :test 'equal))
   (root-component nil :export #t)
   (debug-component-hierarchy #f :type boolean)
   (valid #t :accessor is-frame-valid? :export :accessor)))

(def print-object (frame :identity #t :type #f)
  (print-object-for-string-id-mixin -self-)
  (write-string " index: ")
  (princ (frame-index-of -self-))
  (write-string ", next-index: ")
  (princ (next-frame-index-of -self-))
  (write-string ", actions: ")
  (princ (hash-table-count (action-id->action-of -self-))))

(def (function ei) generate-response-unique-string (&optional prefix context)
  (unless prefix
    (setf prefix "_u"))
  (unless context
    (setf context (or *frame* *response*)))
  (bind ((*print-pretty* #f))
    (with-output-to-string (str)
      (princ prefix str)
      (princ (incf (unique-counter-of context)) str))))

(def (function ei) generate-frame-unique-string (&optional prefix frame)
  (unless prefix
    (setf prefix "_u"))
  (unless frame
    (setf frame *frame*))
  (generate-response-unique-string prefix frame))

(def function %expand-with-frame-unique-strings (generator names body)
  `(let ,(mapcar (lambda (name)
                   (bind (((:values symbol string) (etypecase name
                                                     (symbol
                                                      (values name nil))
                                                     ((cons symbol (cons string-designator null))
                                                      (values (first name) (string (second name)))))))
                     `(,symbol (,generator ,string))))
                 names)
     ,@body))

(def (macro e) with-frame-unique-strings ((&rest names) &body body)
  (%expand-with-frame-unique-strings 'generate-frame-unique-string names body))

(def (macro e) with-response-unique-strings ((&rest names) &body body)
  (%expand-with-frame-unique-strings 'generate-response-unique-string names body))

#+nil ;; TODO this should be a generic if needed at all
(def function invalidate-frame (frame)
  (setf (is-frame-valid? frame) #f)
  (setf (root-component-of frame) nil) ; TODO push down to subclass
  (clrhash (action-id->action-of frame))
  (clrhash (client-state-sink-id->client-state-sink-of frame))
  frame)

(def function is-frame-alive? (frame)
  (cond
    ((not (is-frame-valid? frame)) (values #f :invalidated))
    ((is-timed-out? frame) (values #f :timed-out))
    (t (values #t nil))))

(def (function o) find-frame-for-request (session)
  (bind ((frame-id (parameter-value +frame-id-parameter-name+))
         (frame nil)
         (frame-instance nil)
         (invalidity-reason nil))
    (when frame-id
      (app.debug "Found frame-id parameter ~S" frame-id)
      (setf frame-instance (gethash frame-id (frame-id->frame-of session)))
      (setf frame frame-instance)
      (when frame
        (bind ((alive? #f))
          (setf (values alive? invalidity-reason) (is-frame-alive? frame))
          (if alive?
              (app.debug "Looked up as valid, alive frame ~A" frame)
              (progn
                (app.debug "Looked up as a frame, but it's not valid anymore due to ~S. It's ~A." invalidity-reason frame)
                (setf frame nil))))))
    (when (and (not frame)
               (not invalidity-reason))
      (setf invalidity-reason :nonexistent))
    (values frame (not (null frame-id)) invalidity-reason frame-instance)))

(def method purge-frames (application (session session))
  (assert-session-lock-held session)
  (bind ((frame-id->frame (frame-id->frame-of session)))
    (iter (for (frame-id frame) :in-hashtable frame-id->frame)
          (when (is-timed-out? frame)
            (remhash frame-id frame-id->frame))))
  (values))

(def function step-to-next-frame-index (frame)
  (app.debug "Stepping to next frame index. From ~S to next ~S, frame is ~A" (frame-index-of frame) (next-frame-index-of frame) frame)
  (bind ((original-frame-index (frame-index-of frame)))
    (setf (frame-index-of frame) (next-frame-index-of frame))
    (setf (next-frame-index-of frame)
          (generate-frame-index (frame-index-of frame)))
    original-frame-index))

(def function revert-step-to-next-frame-index (frame original-frame-index)
  (app.debug "Reverting the step to next frame index. Original ~S to next ~S, frame is ~A" original-frame-index (frame-index-of frame) frame)
  (setf (next-frame-index-of frame) (frame-index-of frame))
  (setf (frame-index-of frame) original-frame-index)
  (values))


;;;;;;;;;;;;;;;;;;;;;;
;;; client state sinks

(def constant +client-state-sink-id-length+ 8)

(def class* client-state-sink (string-id-for-funcallable-mixin)
  ()
  (:metaclass funcallable-standard-class))

(def (macro e) client-state-sink ((value-variable-name) &body body)
  `(register-client-state-sink
    *frame* (make-client-state-sink (,value-variable-name) ,@body)))

(def (macro e) make-client-state-sink ((value-variable-name) &body body)
  `(make-client-state-sink-using-lambda (named-lambda client-state-sink-body (,value-variable-name)
                                          (block nil
                                            ,@body))))

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
      (app.dribble "Registered client-state-sink with id ~S in frame ~A" client-state-sink-id frame)))
  client-state-sink)

(def function process-client-state-sinks (frame query-parameters)
  (bind ((client-state-sink-id->client-state-sink (client-state-sink-id->client-state-sink-of frame)))
    (iter (for (name . value) :in query-parameters)
          (app.dribble "Checking query parameter ~S if it's a client-state-sink" name)
          (awhen (gethash name client-state-sink-id->client-state-sink)
            (app.dribble "Query parameter ~S is a client-state-sink, calling it with value ~S" it value)
            (funcall it value)))))
