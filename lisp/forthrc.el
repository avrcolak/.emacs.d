;; A simple rc to configure Forth mode.

(autoload 'forth-mode "gforth" nil t)
(autoload 'forth-block-mode "gforth" nil t)

(add-to-list 'auto-mode-alist '("\\.fs\\'" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fb\\'" . forth-block-mode))

(defun set-forth-indent ()
  (setq forth-indent-level 3)
  (setq forth-minor-indent-level 1))

(add-hook 'forth-mode-hook 'set-forth-indent)
