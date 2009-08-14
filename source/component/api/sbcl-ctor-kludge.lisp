(in-package :sb-pcl)

;;;;;;
;;; components can have component slots through which the parent
;;; relation is automatically updated. by redefining sbcl's ctor code
;;; this kludge makes it possible to use the fast make-instance path
;;; by special casing and extending the ctor form with the required
;;; forms to set up the parent slots of components.
;;;
;;; KLUDGE: makes possible to use optimized constructors for component-classes
;;; KLUDGE: handles only the MOP overrides (setf svuc) :after and initialize-instance :after

;;;;;
;;; KLUDGE: copied over from SBCL source and specialized for component-class

(defun constructor-function-form (ctor)
  (let* ((class (ctor-class ctor))
         (proto (class-prototype class))
         (make-instance-methods
          (compute-applicable-methods #'make-instance (list class)))
         (allocate-instance-methods
          (compute-applicable-methods #'allocate-instance (list class)))
         ;; I stared at this in confusion for a while, thinking
         ;; carefully about the possibility of the class prototype not
         ;; being of sufficient discrimiating power, given the
         ;; possibility of EQL-specialized methods on
         ;; INITIALIZE-INSTANCE or SHARED-INITIALIZE.  However, given
         ;; that this is a constructor optimization, the user doesn't
         ;; yet have the instance to create a method with such an EQL
         ;; specializer.
         ;;
         ;; There remains the (theoretical) possibility of someone
         ;; coming along with code of the form
         ;;
         ;; (defmethod initialize-instance :before ((o foo) ...)
         ;;   (eval `(defmethod shared-initialize :before ((o foo) ...) ...)))
         ;;
         ;; but probably we can afford not to worry about this too
         ;; much for now.  -- CSR, 2004-07-12
         (ii-methods
          (compute-applicable-methods #'initialize-instance (list proto)))
         (si-methods
          (compute-applicable-methods #'shared-initialize (list proto t)))
         (setf-svuc-slots-methods
          (loop for slot in (class-slots class)
                collect (compute-applicable-methods
                         #'(setf slot-value-using-class)
                         (list nil class proto slot))))
         (sbuc-slots-methods
          (loop for slot in (class-slots class)
                collect (compute-applicable-methods
                         #'slot-boundp-using-class
                         (list class proto slot)))))
    ;; Cannot initialize these variables earlier because the generic
    ;; functions don't exist when PCL is built.
    (when (null *the-system-si-method*)
      (setq *the-system-si-method*
            (find-method #'shared-initialize
                         () (list *the-class-slot-object* *the-class-t*)))
      (setq *the-system-ii-method*
            (find-method #'initialize-instance
                         () (list *the-class-slot-object*))))
    ;; Note that when there are user-defined applicable methods on
    ;; MAKE-INSTANCE and/or ALLOCATE-INSTANCE, these will show up
    ;; together with the system-defined ones in what
    ;; COMPUTE-APPLICABLE-METHODS returns.
    (let ((maybe-invalid-initargs
           (check-initargs-1
            class
            (append
             (ctor-default-initkeys
              (ctor-initargs ctor) (class-default-initargs class))
             (plist-keys (ctor-initargs ctor)))
            (append ii-methods si-methods) nil nil))
          (custom-make-instance
           (not (null (cdr make-instance-methods)))))
      (if (and (not (structure-class-p class))
               (not (condition-class-p class))
               (not custom-make-instance)
               (null (cdr allocate-instance-methods))
               (every (lambda (x)
                        (member (slot-definition-allocation x)
                                '(:instance :class)))
                      (class-slots class))
               (not maybe-invalid-initargs)
               (not (around-or-nonstandard-primary-method-p
                     ii-methods *the-system-ii-method*))
               (not (around-or-nonstandard-primary-method-p
                     si-methods *the-system-si-method*))
               ;; the instance structure protocol goes through
               ;; slot-value(-using-class) and friends (actually just
               ;; (SETF SLOT-VALUE-USING-CLASS) and
               ;; SLOT-BOUNDP-USING-CLASS), so if there are non-standard
               ;; applicable methods we can't shortcircuit them.
               ;; KLUDGE: specialization for component-class
               (or (every (lambda (x) (= (length x) 1)) setf-svuc-slots-methods)
                   (typep class 'hu.dwim.wui::component-class))
               (every (lambda (x) (= (length x) 1)) setf-svuc-slots-methods)
               (every (lambda (x) (= (length x) 1)) sbuc-slots-methods))
          (optimizing-generator ctor ii-methods si-methods)
          (fallback-generator ctor ii-methods si-methods
                              (or maybe-invalid-initargs custom-make-instance))))))

;;;;;;
;;; KLUDGE: copied over from SBCL source and specialized for component-class

(defun wrap-in-allocate-forms (ctor body before-method-p)
  (let* ((class (ctor-class ctor))
         (wrapper (class-wrapper class))
         (allocation-function (raw-instance-allocator class))
         (slots-fetcher (slots-fetcher class)))
    (if (eq allocation-function 'allocate-standard-instance)
        `(let ((.instance. (%make-standard-instance nil
                                                    (get-instance-hash-code)))
               (.slots. (make-array
                         ,(layout-length wrapper)
                         ,@(when before-method-p
                             '(:initial-element +slot-unbound+)))))
           (setf (std-instance-wrapper .instance.) ,wrapper)
           (setf (std-instance-slots .instance.) .slots.)
           ,body
           ;; KLUDGE: specialization for component-class
           ,@(loop
                :for slot :in (class-slots class)
                :when (typep slot 'hu.dwim.wui::component-effective-slot-definition)
                :collect `(setf (hu.dwim.wui::parent-component-references .instance.) (standard-instance-access .instance. ,(slot-definition-location slot))))
           .instance.)
        `(let* ((.instance. (,allocation-function ,wrapper))
                (.slots. (,slots-fetcher .instance.)))
           (declare (ignorable .slots.))
           ,body
           .instance.))))

