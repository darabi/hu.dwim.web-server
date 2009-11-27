(in-package :hu.dwim.wui)

;;;;;;
;;; A simple task scheduler

(def class* timer ()
  ((entries nil)
   (lock (make-recursive-lock "Timer lock"))
   (running-thread nil)
   (shutdown-initiated #f :type boolean)
   (condition-variable (make-condition-variable))))

(def with-macro with-lock-held-on-timer (timer)
  (multiple-value-prog1
      (progn
        (threads.dribble "Entering with-lock-held-on-timer for timer ~S in thread ~S" timer (current-thread))
        (with-recursive-lock-held ((lock-of timer))
          (-body-)))
    (threads.dribble "Leaving with-lock-held-on-timer for timer ~S in thread ~S" timer (current-thread))))

(def function drive-timer (timer)
  "This is the entry point of the timer. There should be one and only one thread calling this function for each timer."
  (flet ((reschedule-entries ()
           (setf (entries-of timer)
                 (sort (delete-if (complement #'timer-entry-valid?) (entries-of timer))
                       'local-time:timestamp<
                       :key 'scheduled-at-of))))
    (timer.debug "Thread is entering the timer loop DRIVE-TIMER of ~A" timer)
    (unwind-protect
         (progn
           (assert (null (running-thread-of timer)))
           (setf (running-thread-of timer) (current-thread))
           (setf (shutdown-initiated-p timer) #f)
           (with-simple-restart (abort-timer "Break out from the timer loop")
             (loop
                :until (shutdown-initiated-p timer) :do
                (bind ((entries)
                       (run-anything? #f))
                  (with-lock-held-on-timer timer
                    (timer.dribble "~A sorting ~A entries" timer (length (entries-of timer)))
                    ;; need to copy, because we will release the lock before processing finishes
                    (setf entries (copy-list (reschedule-entries))))
                  (dolist (entry entries)
                    (when (local-time:timestamp< (scheduled-at-of entry) (local-time:now))
                      (setf run-anything? #t)
                      (run-timer-entry entry)))
                  (unless run-anything?
                    (with-lock-held-on-timer timer
                      (bind ((first-entry (first (reschedule-entries)))
                             (expires-in (or (when first-entry
                                               (local-time:timestamp-difference (scheduled-at-of first-entry) (local-time:now)))
                                             ;; this is an ad-hoc large constant to keep the code path uniform. would be safe to wake up though...
                                             (* 60 60 24 365 10))))
                        (timer.dribble "~A will fall asleep for ~A seconds" timer expires-in)
                        (when (plusp expires-in)
                          (handler-case
                              (with-timeout (expires-in)
                                (condition-wait (condition-variable-of timer) (lock-of timer))
                                (timer.dribble "~A woke up from CONDITION-WAIT" timer))
                            (timeout ()
                              (timer.dribble "~A woke up from CONDITION-WAIT due to the timeout" timer)))))))))))
      (setf (running-thread-of timer) nil)
      (with-lock-held-on-timer timer
        (condition-notify (condition-variable-of timer)))
      (timer.debug "Thread is leaving the timer loop DRIVE-TIMER of ~A" timer))))

(def (function e) drive-timer/abort ()
  (invoke-restart 'abort-timer)
  (error "It should be impossible to get here..."))

(def function shutdown-timer (timer &key (wait #t))
  (if wait
      (loop
         :while (running-thread-of timer) :do
         (with-lock-held-on-timer timer
           (setf (shutdown-initiated-p timer) #t)
           (condition-wait (condition-variable-of timer) (lock-of timer))))
      (setf (shutdown-initiated-p timer) #t)))

(def (function e) register-timer-entry (timer time thunk &key (kind :periodic) (name "<unnamed>"))
  (check-type kind (member :periodic :single-shot))
  (timer.debug "Registering timer entry ~S for timer ~A, at time ~A, kind ~S, thunk ~A" name timer time kind thunk)
  (with-lock-held-on-timer timer
    (push (ecase kind
            (:periodic
             (unless (numberp time)
               (error "The TIME argument must be the interval length in seconds for periodic timers"))
             (make-instance 'periodic-timer-entry
                            :name name
                            :scheduled-at (local-time:now)
                            :interval time
                            :thunk thunk))
            (:single-shot
             (unless (typep time 'local-time:timestamp)
               (error "The TIME argument must be a local-time timestamp denoting the time when this single-shot timer should be run"))
             (make-instance 'single-shot-timer-entry
                            :name name
                            :scheduled-at time
                            :thunk thunk)))
          (entries-of timer))
    (timer.debug "Waking up timer ~A because of a new entry" timer)
    (condition-notify (condition-variable-of timer))))

(def function timer-entry-valid? (entry)
  (not (null (thunk-of entry))))

(def class* timer-entry ()
  ((name (mandatory-argument) :type string)
   (scheduled-at :type local-time:timestamp)
   (thunk (mandatory-argument) :type (or symbol function))))

(def print-object (timer-entry :identity #f)
  (prin1 (name-of -self-)))

(def class* single-shot-timer-entry (timer-entry)
  ())

(def class* periodic-timer-entry (timer-entry)
  ((interval (mandatory-argument))))

(def generic run-timer-entry (entry)
  (:method ((entry timer-entry))
    (timer.dribble "Running timer entry ~A" entry)
    (awhen (thunk-of entry)
      (handler-bind
          ((serious-condition (lambda (level-1-error)
                                (handler-bind
                                    ((serious-condition (lambda (level-2-error)
                                                          (ignore-errors
                                                            (wui.error "Oops, nested error while running timer entry. Level1 error type: ~S, level2 error type: ~S"
                                                                       entry (type-of level-1-error) (type-of level-2-error))))))
                                  (wui.error "Error while running timer entry ~A: ~A" entry level-1-error)))))
        (with-simple-restart (skip-timer-entry "Skip calling timer entry ~A" entry)
          (funcall it)))))

  (:method :after ((entry single-shot-timer-entry))
    (timer.debug "Invalidating single shot timer entry ~A" entry)
    (setf (thunk-of entry) nil))

  (:method :after ((self periodic-timer-entry))
    (setf (scheduled-at-of self) (local-time:adjust-timestamp (local-time:now) (offset :sec (interval-of self))))))
