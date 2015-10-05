;;; A simple rc to configure slime.

(y-require 'slime)

;; Use M-x slime with a negative prefix to choose between
;; implementations i.e. M-- M-x slime.
(setq slime-lisp-implementations
      '((sbcl ("sbcl"))
        (ccl ("ccl64"))))

(slime-setup '(slime-fancy slime-banner inferior-slime))

(define-key lisp-mode-map (kbd "C-c s") 'y-slime-send-dwim)

;; Prefer rocker clicks to horizontal scrolling for inspector mouse
;; control.
(define-key slime-inspector-mode-map (kbd "<mouse-6>") nil)
(define-key slime-inspector-mode-map (kbd "<mouse-7>") nil)
(define-key slime-inspector-mode-map (kbd "<mouse-8>") 'slime-inspector-next)
(define-key slime-inspector-mode-map (kbd "<mouse-9>") 'slime-inspector-pop)

(define-key slime-repl-mode-map
  (kbd "<mouse-1>") 'slime-inspect-presentation-at-mouse)

(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

;; Unicode characters from and to Lisp.
(setq slime-net-coding-system 'utf-8-unix)

;; HACK: Don't clobber Paredit's backspace.
(define-key slime-repl-mode-map "" nil)

;; Use Quicklisp's HyperSpec package if available.
(let ((clhs-use-local (expand-file-name "~/quicklisp/clhs-use-local.el")))
  (when (file-exists-p clhs-use-local)
    (load clhs-use-local)
    ;; HACK: clhs-use-local.el does not setup
    ;; `common-lisp-hyperspec-root' robustly with respect to some
    ;; possible `browse-url' values. So patch it.
    (setq common-lisp-hyperspec-root
          (replace-regexp-in-string "file:/[^/]??"
                                    "file:///"
                                    common-lisp-hyperspec-root))))
