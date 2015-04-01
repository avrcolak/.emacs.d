;;; A simple rc to configure SLIME.

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;;; SLIME signals an error on load when loaded with a sufficiently old
;;; version of Emacs, which stops the rest of Emacs'
;;; initialization. Failing silently is preferrable.
(when (condition-case nil
          (require 'slime nil t)
        (error nil))

  ;; Use M-x slime with a negative prefix to choose between
  ;; implementations i.e. M-- M-x slime.
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (ccl ("ccl64"))))

  ;; The warnings aren't spurious, but aren't worth raising a buffer
  ;; over.
  (let ((warning-minimum-level :emergency))
    (slime-setup '(slime-fancy slime-banner inferior-slime)))

  (global-set-key (kbd "<f12>") 'slime-selector)
  (global-set-key (kbd "C-c s") 'yard-slime-send-dwim)

  ;; Use fuzzy completion with M-TAB.
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  
  ;; Unicode characters from and to Lisp.
  (setq slime-net-coding-system 'utf-8-unix)

  ;; Don't clobber Paredit's backspace.
  (defun yard-slime-paredit-kludge ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key)
      nil))

  (add-hook 'slime-mode-hook 'paredit-mode)
  (add-hook 'slime-mode-hook 'redshank-mode)
  (add-hook 'slime-repl-mode-hook 'paredit-mode)
  (add-hook 'slime-repl-mode-hook 'redshank-mode)
  ;; (add-hook 'slime-repl-mode-hook 'yard-set-common-lisp-indent)
  (add-hook 'slime-repl-mode-hook 'yard-slime-paredit-kludge)
  (add-hook 'slime-inferior-process-start-hook 'paredit-mode)
  (add-hook 'slime-inferior-process-start-hook 'redshank-mode)

  ;; lisp-mode clobbers slime-edit-mode bindings so get rid of it.
  ;; (set-keymap-parent slime-repl-mode-map nil)

  ;; Use Quicklisp's HyperSpec package if available.
  (let ((clhs (expand-file-name "~/quicklisp/clhs-use-local.el")))
    (when (file-exists-p clhs)
      (load clhs))))
