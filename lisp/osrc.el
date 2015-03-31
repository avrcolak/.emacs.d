;;; A simple rc to configure Emacs' interface with the host OS.

;;; No Emacs droppings.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Use clobbering sessions.
(setq desktop-dirname "~/.emacs.d/"
      desktop-save t
      desktop-load-locked-desktop t)

(desktop-save-mode)

;;; Restoring a session sometimes opens a file with unsafe file local
;;; variables. Emacs' default behavior is to ask the user for
;;; confirmation except when run in batch mode where "it can't really
;;; ask you so it assumes the answer n". "it can't really ask you" is
;;; also the case in some startup situations, but Emacs still does
;;; anyway, which blocks the startup process. Specifically, in the
;;; situation where desktop-read is executed before window-setup-hook
;;; (as is the case when desktop-save-mode is turned on), Emacs should
;;; not ask the user about unsafe file local variables. Hence this
;;; hack.
(setq enable-local-variables :safe)
(add-hook 'after-init-hook (lambda () (setq enable-local-variables t)))

;;; User local info manuals.
(eval-after-load "info"
  '(add-to-list 'Info-additional-directory-list "~/share/info/"))

;;; Non system site-lisp for a system Emacs.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(prefer-coding-system 'utf-8)

;;; browse-url-firefox does not do the right thing on OS X. Furthemore
;;; Safari is preferred.
(if (eq system-type 'darwin)
    (setq browse-url-generic-program "open"
          browse-url-generic-args '("-a" "safari"))
    (setq browse-url-browser-function 'browse-url-firefox))

;;; More useful is to reuse the existing frame when responding to
;;; GNUstep or Cocoa open file events.
(setq ns-pop-up-frames nil)

;;; Oddly, the environment PATH and exec-path may differ after loading
;;; e.g. on Mac OS X.
(setq exec-path (delete-dups (append (yard-get-environment-path) exec-path)))

(global-set-key (kbd "C-c r") 'yard-root-find-file)
(global-set-key (kbd "C-c c") 'save-buffers-kill-emacs)
