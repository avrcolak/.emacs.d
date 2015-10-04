;;; An Emacs initialization file. The heavy lifting is done in the
;;; loaded Emacs Lisp files.

;;; Save customizations somewhere else.
(setq custom-file "~/.emacs.d/.custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "yard")
(setq load-path
      (append (yard-find-subdirectories '("~/.emacs.d/lisp")) load-path))
(load "os-rc")
(load "ui-rc")
(load "edit-rc")
(load "smex-rc")
(load "ido-ubiquitous-rc")
(load "switch-window-rc")
(load "paredit-rc")
(load "mouse-copy-rc")
(load "org-rc")
(load "slime-rc")
(load "redshank-rc")
(load "imaxima-rc")
(load "wolfram-mode-rc")
(load "gambit-rc")
(load "moz-rc")
(load "markdown-rc")
(load "go-mode-rc")
(load "go-complete-rc")
(load "go-eldoc-rc")
