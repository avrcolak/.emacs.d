;;; A simple rc to set up Trident Mode.

(add-to-list 'auto-mode-alist (cons "\\.paren\\'" 'lisp-mode))
(add-to-list 'yard-auto-minor-mode-alist (cons "\\.paren\\'" 'trident-mode))

(add-hook 'trident-mode-hook
          (lambda ()
            ;; No collisions with SLIME, Paredit or Redshank.
            (trident-add-keys-with-prefix "C-c C-i")))
