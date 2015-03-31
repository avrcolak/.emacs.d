;;; A simple rc to configure Emacs to edit and interact with GNU
;;; Smalltalk.

;;; On some systems smalltalk-mode will not have been added to the
;;; autoloads list (e.g. those without a site-start.el and
;;; site-start.d directory).
(when (not (fboundp 'smalltalk-mode))
  (require 'smalltalk-mode nil 'noerror))
