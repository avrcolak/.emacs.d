;;; A simple rc to configure slime.

(y-pseudo-require 'slime)

;; Use M-x slime with a negative prefix to choose between
;; implementations i.e. M-- M-x slime.
(setq slime-lisp-implementations
      '((sbcl ("sbcl"))
        (ccl ("ccl64"))))

(defun y-slime-send-dwim (arg)
  "Send the code form you want to the buffer named by
`slime-output-buffer' (Do What I Mean). If a region is active, it
is saved and yanked to the buffer. Else, if the point is at an
opening paren, the sexp immediately following the point is saved
and yanked. Else, if the point is directly after a closing paren,
the sexp immediately preceding the point is saved and yanked.
Else, the top level sexp enclosing the point is saved and yanked.

With ARG, evaluate the resulting output buffer input string."
  (interactive "P")
  (save-excursion
    (cond (mark-active
           (copy-region-as-kill (mark) (point)))
          ((eq (char-after) ?\()
           (let ((beg (point))
                 (end (save-excursion (forward-sexp) (point))))
             (copy-region-as-kill beg end)))
          ((eq (char-before) ?\))
           (let ((end (point))
                 (beg (save-excursion (backward-sexp) (point))))
             (copy-region-as-kill beg end)))
          (t
           (let* ((beg (progn (beginning-of-defun)
                              (point)))
                 (end (save-excursion (end-of-defun) (point))))
             (copy-region-as-kill beg end))))
      (switch-to-buffer-other-window (slime-output-buffer))
      (goto-char (point-max))
      (when (string-match "\n\\|" (car kill-ring))
        (slime-repl-newline-and-indent))
      (yank)
      (when arg
        (slime-repl-return))))

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy slime-banner inferior-slime))

     (define-key lisp-mode-map (kbd "C-c s") 'y-slime-send-dwim)

     ;; Prefer rocker clicks to horizontal scrolling for inspector mouse
     ;; control.
     (define-key slime-inspector-mode-map (kbd "<mouse-6>") nil)
     (define-key slime-inspector-mode-map (kbd "<mouse-7>") nil)
     (define-key slime-inspector-mode-map (kbd "<mouse-5>")
       'slime-inspector-pop)
     (define-key slime-inspector-mode-map (kbd "<mouse-4>")
       'slime-inspector-next)

     (define-key slime-repl-mode-map
       (kbd "<mouse-1>") 'slime-inspect-presentation-at-mouse)

     (setq slime-inspector-insert-ispec-function
           'slime-presentation-inspector-insert-ispec)

     (setq display-buffer-alist
           '(("^\\*sldb" . (y-display-buffer-previous-window))))

     ;; Rearranging the documentation for clarity: "Coding system used to
     ;; transmit characters between Emacs and the Lisp system.".
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
                                         common-lisp-hyperspec-root))))))
