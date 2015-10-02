;;; An Emacs initialization file. The heavy lifting is done in the
;;; loaded Emacs Lisp files.

;;; Save customizations somewhere else.
(setq custom-file "~/.emacs.d/.custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "yard")
(load "os-rc")
(load "ui-rc")
(load "edit-rc")
(load "ido-rc")
(load "smex-rc")
(load "ido-ubiquitous-rc")
(load "switch-window-rc")
(load "paredit-rc")
(load "term-rc")
(load "org-rc")
(load "slime-rc")
(load "redshank-rc")
(load "imaxima-rc")
(load "wolfram-mode-rc")
(load "gambit-rc")
(load "moz-rc")
(load "markdown-rc")
