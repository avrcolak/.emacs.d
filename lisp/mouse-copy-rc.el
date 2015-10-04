;;; Simple rc to set up mouse-copy.

(when (yard-require 'mouse-copy)
  (add-hook 'paredit-mode-hook 'mouse-copy-mode)

  ;; No need for mouse-copy to clobber these.
  (define-key mouse-mode-map (kbd "<C-mouse-1>") nil)
  (define-key mouse-mode-map (kbd "<C-drag-mouse-1>") nil)
  (define-key mouse-mode-map (kbd "<C-down-mouse-1>") nil)

  ;; Instead use the common prefix for operations over S-expressions
  ;; (C-M) and common button for mouse insertion in X11 (2).
  (define-key mouse-mode-map (kbd "<C-M-mouse-2>")
    'mouse-insert-sexp-at-point))
