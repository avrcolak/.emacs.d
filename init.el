;;; An Emacs initialization file. The heavy lifting is done in the
;;; loaded Emacs Lisp files.

;; "The default is nil, which means to use your init file".
(setq custom-file "~/.emacs.d/.custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(setq package-archives
      '(("elpa.gnu.org" . "https://elpa.gnu.org/packages/")
        ("stable.melpa.org" . "https://stable.melpa.org/packages/")
        ("melpa.org" . "https://melpa.org/packages/")
        ("orgmode.org" . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("elpa.gnu.org" . 10)
        ("stable.melpa.org" . 5)
        ("melpa.org" . 0)
        ("orgmode.org" . 15)))

;; Setup package load paths.
(when (require 'package nil t)
  (package-initialize))

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "y")
(setq load-path
      (append (y-find-subdirectories '("~/.emacs.d/lisp")) load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")

(let ((debug-on-error nil))
  (dolist (rc '("os-rc"
                "ui-rc"
                "edit-rc"
                "smex-rc"
                "ace-window-rc"
                "paredit-rc"
                "mouse-copy-rc"
                "org-rc"
                "slime-rc"
                "redshank-rc"
                "imaxima-rc"
                "wolfram-mode-rc"
                "gambit-rc"
                "rainbow-mode-rc"))
    (with-demoted-errors "Error: %S" (load rc))))
