;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (function o) parse-accept-language-header (header-string)
  (declare (type simple-base-string header-string))
  (http.dribble "Parsing Accept header ~S." header-string)
  (flet ((make-string-buffer ()
           (make-array 2 :element-type 'character :adjustable t :fill-pointer 0))
         (without-chars (string &optional (char-bag '(#\Space #\Tab #\Newline #\Linefeed)))
           (with-output-to-string (new-string)
             (loop for char across string
                   unless (member char char-bag)
                   do (write-char char new-string)))))
    (let ((accepts '())
          (lan (make-string-buffer))
          (scr (make-string-buffer))
          (q (make-string-buffer))
          (index 0)
          (header (without-chars header-string)))
      (declare (type (integer 0 (#.array-dimension-limit)) index))
      (labels ((next-char ()
                 (unless (<= (length header) index)
                   (aref header index)))
               (read-next-char ()
                 (prog1
                     (next-char)
                   (incf index)))
               (next-accept ()
                 (push (list lan scr q) accepts)
                 (setf lan (make-string-buffer)
                       scr (make-string-buffer)
                       q (make-string-buffer))))
        (loop
            with state = :lan
            for char = (read-next-char)
            while char
            do (case state
                 (:lan
                  (case char
                    (#\- (setf state :scr))
                    (#\; (setf state :q))
                    (#\, (setf state :lan)
                         (next-accept))
                    (t (vector-push-extend char lan)
                       (unless (next-char)
                         (next-accept)))))
                 (:scr
                  (case char
                    (#\; (setf state :q))
                    (#\, (setf state :lan) (next-accept))
                    (t (vector-push-extend char scr)
                       (unless (next-char)
                         (next-accept)))))
                 (:q
                  (read-next-char)
                  (loop
                      for char = (read-next-char)
                      while char
                      until (char= #\, char)
                      do (vector-push-extend char q))
                  (setf state :lan)
                  (next-accept)))))
      (sort (mapcar (lambda (acc)
                      (when (string= "" (first acc))
                        (error "Badly formatted accept-languages header."))
                      (let ((quality (if (not (string= "" (third acc)))
                                         (parse-number:parse-number (third acc))
                                         1)))
                        (if (string= "" (second acc))
                            (list (first acc) quality)
                            (list (concatenate 'string (first acc) "_" (string-upcase (second acc)))
                                  quality))))
                    (nreverse accepts))
            #'> :key #'second))))
