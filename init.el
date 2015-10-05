;;; An Emacs initialization file. The heavy lifting is done in the
;;; loaded Emacs Lisp files.

;;; Save customizations somewhere else.
(setq custom-file "~/.emacs.d/.custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "yard")
(setq load-path
      (append (y-find-subdirectories '("~/.emacs.d/lisp")) load-path))

(let ((debug-on-error nil))
  (dolist (rc '("os-rc"
                "ui-rc"
                "edit-rc"
                "smex-rc"
                "ido-ubiquitous-rc"
                "switch-window-rc"
                "paredit-rc"
                "mouse-copy-rc"
                "org-rc"
                "slime-rc"
                "redshank-rc"
                "imaxima-rc"
                "wolfram-mode-rc"
                "gambit-rc"
                "moz-rc"
                "markdown-rc"
                "go-mode-rc"
                "go-complete-rc"
                "go-eldoc-rc"))
    (with-demoted-errors "Error: %S" (load rc))))
