;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def resources en
  (mime-type.application/msword "Microsoft Word Document")
  (mime-type.application/vnd.ms-excel "Microsoft Excel Document")
  (mime-type.application/pdf "PDF Document")
  (mime-type.image/png "PNG image")
  (mime-type.image/tiff "TIFF image"))

;;; Error handling
(def resources en
  (error.internal-server-error "Internal server error")
  (render-internal-error-page (&rest args &key &allow-other-keys)
    (apply 'render-internal-error-page/english args))

  (error.access-denied-error "Access denied")
  (render-access-denied-error-page (&rest args &key &allow-other-keys)
    (apply 'render-access-denied-error-page/english args)))

;;; Context sensitive help
(def resources en
  (icon-label.help "Help")
  (help.no-context-sensitive-help-available "No conext sensitive help available")
  (help.help-about-context-sensitive-help-button "This is the switch that can be used to turn on the context sensitive help. In help mode hovering the mouse over certain parts of the screen opens a tooltip just like this, but containing the most relevant help to that point (in this case the description of the help mode itself). A special mouse pointer indicates help mode. Clicking the mouse button anywhere in help mode turns off the mode."))