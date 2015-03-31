;;; An Emacs initialization file. The heavy lifting is done in the
;;; loaded Elisp files.

;;; Save customizations somewhere else.
(setq custom-file "~/.emacs.d/customrc.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "yard.el")
(load "osrc.el")
(load "uirc.el")
(load "editrc.el")
(load "packrc.el")
;; (load "cedetrc.el")
(load "geiserrc.el")
(load "gambitrc.el")
(load "skewerrc.el")
(load "tridentrc.el")
(load "mozreplrc.el")
(load "maximarc.el")
(load "orgrc.el")
(load "slimerc.el")
(load "smalltalkrc.el")
(load "mathematicarc.el")
(load "forthrc.el")
(load "vbrc.el")
(load "turtlerc.el")
