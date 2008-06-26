(in-package :hu.dwim.wui)

(dojo.get-object "wui" #t)

(defun* wui.submit-form (href)
  (let ((form (aref (slot-value document 'forms) 0)))
    (setf (slot-value form 'action) href)
    (form.submit)))
