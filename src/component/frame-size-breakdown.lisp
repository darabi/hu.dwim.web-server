;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Frame size component

(def component frame-size-breakdown-component ()
  ((last-dynamic-usage 0)
   (last-descriptors nil)))

(def render-xhtml frame-size-breakdown-component
  (sb-ext:gc :full t)
  ;; TODO: room
  (bind ((last-dynamic-usage (last-dynamic-usage-of -self-))
         (new-dynamic-usage (sb-kernel:dynamic-usage))
         (last-descriptors (last-descriptors-of -self-))
         (new-descriptors
          (collect-object-size-descriptors-for-retained-objects
           (root-component-of *frame*) :ignored-type '(or symbol
                                                          standard-class
                                                          standard-slot-definition
                                                          standard-generic-function
                                                          sb-vm::code-component)))
         (last-total-count 0)
         (last-total-size 0)
         (new-total-count 0)
         (new-total-size 0)
         (class-names (delete-duplicates
                       (append (when last-descriptors
                                 (mapcar #'class-name-of last-descriptors))
                               (mapcar #'class-name-of new-descriptors)))))
    (setf (last-dynamic-usage-of -self-) new-dynamic-usage)
    (setf (last-descriptors-of -self-) new-descriptors)
    (labels ((last-descriptor-for (class-name)
               (find class-name last-descriptors :key #'class-name-of))
             (new-descriptor-for (class-name)
               (find class-name new-descriptors :key #'class-name-of))
             (average (size count)
               (if (zerop count)
                   "N/A"
                   (coerce (/ size count) 'float)))
             (render-cells (name last-count new-count last-size new-size)
               <td ,name>
               <td (:class "new") ,new-size>
               <td (:class "last") ,last-size>
               <td (:class "delta") ,(- new-size last-size)>
               <td (:class "new") ,new-count>
               <td (:class "last") ,last-count>
               <td (:class "delta") ,(- new-count last-count)>
               <td (:class "new") ,(average new-size new-count)>
               <td (:class "last") ,(average last-size last-count)>))
      <table (:class "frame-size-breakdown")
        <thead <th "New dynamic usage">
               <th "Last dynamic usage">
               <th "Dynamic usage delta">>
        <tbody <tr <td (:class "new") ,(format nil "~10:D bytes" new-dynamic-usage)>
                   <td (:class "last") ,(format nil "~10:D bytes" last-dynamic-usage)>
                   <td (:class "delta") ,(format nil "~10:D bytes" (- new-dynamic-usage last-dynamic-usage))>>>>
      <table (:class "frame-size-breakdown")
        <thead <th "Class name">
               <th "New total size">
               <th "Last total size">
               <th "Total size delta">
               <th "New count">
               <th "Last count">
               <th "Count delta">
               <th "New average size">
               <th "Last average size">>
        <tbody ,(iter (for index :from 0)
                      (for class-name :in (sort class-names (find-symbol ">" :cl) ;; FIXME: syntax-sugar breaks reading without find-symbol
                                                :key (lambda (class-name)
                                                       (aif (new-descriptor-for class-name)
                                                            (size-of it)
                                                            0))))
                      (for last-descriptor = (when last-descriptors (last-descriptor-for class-name)))
                      (for last-count = (if last-descriptor
                                            (count-of last-descriptor)
                                            0))
                      (for last-size = (if last-descriptor
                                           (size-of last-descriptor)
                                           0))
                      (incf last-total-count last-count)
                      (incf last-total-size last-size)
                      (for new-descriptor = (new-descriptor-for class-name))
                      (for new-count = (if new-descriptor
                                           (count-of new-descriptor)
                                           0))
                      (for new-size = (if new-descriptor
                                          (size-of new-descriptor)
                                          0))
                      (incf new-total-count new-count)
                      (incf new-total-size new-size)
                      <tr (:class ,(if (oddp index) "odd-row" "even-row"))
                          ,(render-cells (string-downcase class-name) last-count new-count last-size new-size)>)
               <tr (:class "total-row")
                   ,(render-cells "Total" last-total-count new-total-count last-total-size new-total-size)>>>)))
