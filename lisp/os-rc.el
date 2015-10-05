;;; A simple rc to configure Emacs' interface with the host OS.

;; No ad-hoc (sometimes redundant) versioning.
(setq make-backup-files nil)

;; Use clobbering sessions.
(setq desktop-dirname "~/.emacs.d/"
      desktop-save t
      desktop-load-locked-desktop t)

(desktop-save-mode)

;; Restoring a session sometimes opens a file with unsafe file local
;; variables. Emacs' default behavior is to ask the user for
;; confirmation except when run in batch mode where "it can't really
;; ask you so it assumes the answer n". "it can't really ask you" is
;; also the case in some startup situations, but Emacs still does
;; anyway, which blocks the startup process. Specifically, in the
;; situation where desktop-read is executed before window-setup-hook
;; (as is the case when desktop-save-mode is turned on), Emacs should
;; not ask the user about unsafe file local variables. Hence this
;; hack.
(setq enable-local-variables :safe)
(add-hook 'after-init-hook (lambda () (setq enable-local-variables t)))

;; User local info manuals.
(eval-after-load "info"
  '(add-to-list 'Info-additional-directory-list "~/share/info/"))

;; Non system site-lisp for a system Emacs.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; Packages should be loadable without the package feature.
(let ((default-directory "~/.emacs.d/elpa"))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(prefer-coding-system 'utf-8)

;; More useful is to reuse the existing frame when responding to
;; GNUstep or Cocoa open file events.
(setq ns-pop-up-frames nil)

;; Oddly, the environment PATH and exec-path may differ after loading
;; e.g. on Mac OS X.
(setq exec-path (delete-dups (append (y-get-environment-path) exec-path)))
